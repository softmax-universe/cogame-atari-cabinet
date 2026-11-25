## Strict canonical stance parsing and the RUNE discipline.

import std/[json, strutils, unicode, unittest]
import cabinet/[sim, stances]

const
  NoneOut: array[CabinetCount, bool] = [false, false, false, false]
  BothLive = @[true, true]

proc parse(text: string, myCabinet = 2, cabinetOut = NoneOut,
    live = BothLive, catchEnabled = true): CabinetStance =
  parseCabinetStance(extractJsonObject(text), myCabinet, cabinetOut, live,
    catchEnabled)

proc validJson(overrides: openArray[(string, JsonNode)] = @[]): string =
  var node = %*{
    "stance": "guard",
    "target_ball": "any",
    "aim_at": "none",
    "post": 0.0,
    "lead_ticks": 12,
    "aggression": 0.8,
    "note": "",
    "say": ""
  }
  for (k, v) in overrides:
    node[k] = v
  $node

suite "stances":
  test "raw JSON and a standard outer Markdown JSON fence both parse":
    let raw = parse(validJson([("stance", %"aim"), ("aim_at", %"BLUE"),
      ("note", %"take the shot")]))
    check raw.stance == stAim
    check raw.aimAt == 1
    check raw.note == "take the shot"

    let fenced = parse("```json\n" & validJson([("stance", %"camp"),
      ("post", %12.5)]) & "\n```")
    check fenced.stance == stCamp
    check fenced.postUu == 125_000

    let plainFence = parse("```\n" & validJson([("stance", %"chase"),
      ("aim_at", %"RED")]) & "\n```")
    check plainFence.stance == stChase
    check plainFence.aimAt == 0

  test "prose-prefixed or suffixed replies are rejected":
    expect StanceError:
      discard parse("Sure! Here is my stance:\n```json\n" & validJson() & "\n```")
    expect StanceError:
      discard parse("```json\n" & validJson() & "\n```\nGood luck!")
    expect StanceError:
      discard parse("I think " & validJson() & " is right")

  test "required fields must all be present and unknown aliases are rejected":
    for missing in ["stance", "target_ball", "aim_at", "post", "lead_ticks", "aggression"]:
      var node = parseJson(validJson())
      node.delete(missing)
      expect StanceError:
        discard parse($node)
    # optional note/say may be omitted
    var minimal = parseJson(validJson())
    minimal.delete("note")
    minimal.delete("say")
    check parse($minimal).stance == stGuard

    for alias in [("intent", %"guard"), ("targetBall", %"B1"),
                  ("aimAt", %"BLUE"), ("leadTicks", %12)]:
      var node = parseJson(validJson())
      node[alias[0]] = alias[1]
      expect StanceError:
        discard parse($node)

  test "percentage, string, boolean, and out-of-range numeric coercions are rejected":
    check parse(validJson([("aggression", %0.5)])).aggression255 == 128
    check parse(validJson([("aggression", %1)])).aggression255 == 255
    check parse(validJson([("post", %43)])).postUu == PaddleTravelHalf
    check parse(validJson([("post", %(-43.0))])).postUu == -PaddleTravelHalf
    for bad in [
      validJson([("aggression", %80)]),
      validJson([("aggression", %(-0.1))]),
      validJson([("aggression", %"0.5")]),
      validJson([("aggression", %true)]),
      validJson([("post", %80)]),
      validJson([("post", %(-999))]),
      validJson([("post", %"12.5")]),
      validJson([("lead_ticks", %49)]),
      validJson([("lead_ticks", %(-1))]),
      validJson([("lead_ticks", %12.5)]),
      validJson([("lead_ticks", %"12")])
    ]:
      expect StanceError:
        discard parse(bad)

  test "aim_at requires an exact alive rival alias or none":
    check parse(validJson([("aim_at", %"RED")])).aimAt == 0
    check parse(validJson([("aim_at", %"none")])).aimAt == -1
    var eliminated = NoneOut
    eliminated[1] = true
    for bad in ["the red cabinet", "red", "nobody", "null", "RED and BLUE"]:
      expect StanceError:
        discard parse(validJson([("aim_at", %bad)]))
    expect StanceError:
      discard parse(validJson([("aim_at", %"GREEN")]), myCabinet = 2)
    expect StanceError:
      discard parse(validJson([("aim_at", %"BLUE")]), cabinetOut = eliminated)

  test "target_ball requires canonical B1..BN (live) or any":
    check parse(validJson([("target_ball", %"B1")])).targetBall == 0
    check parse(validJson([("target_ball", %"B2")])).targetBall == 1
    check parse(validJson([("target_ball", %"any")])).targetBall == -1
    for bad in [%"ball 2", %"2", %2, %"b2", %"all", %"none", %"B3"]:
      expect StanceError:
        discard parse(validJson([("target_ball", bad)]))
    expect StanceError:
      discard parse(validJson([("target_ball", %"B2")]), live = @[true, false])

  test "stance synonyms, case variants, and disabled catch are rejected":
    for bad in ["defend", "shoot", "attack", "hold", "sit", "grab", "rush", " AIM ", "guard ", "waltz"]:
      expect StanceError:
        discard parse(validJson([("stance", %bad)]))
    check parse(validJson([("stance", %"catch")]), catchEnabled = true).stance == stCatch
    expect StanceError:
      discard parse(validJson([("stance", %"catch")]), catchEnabled = false)

  test "a 300-character note is cut to 160 RUNES":
    let long = "x".repeat(300)
    let stance = parse(validJson([("note", %long)]))
    check stance.note.runeLen == MaxNoteRunes
    check stance.note.validateUtf8() == -1

  test "a say whose 48th and 49th characters are a 4-byte emoji cuts on the RUNE boundary":
    # 47 ASCII runes, then a 4-byte emoji at rune 48, then another.
    let say = "a".repeat(47) & "\u{1F3AF}\u{1F3AF}"
    check say.runeLen == 49
    check say.len == 47 + 8
    var stance = defaultStance()
    stance.say = say
    let cut = say.truncateRunes(MaxSayRunes)
    check cut.runeLen == MaxSayRunes
    check cut.validateUtf8() == -1
    # the sanitiser drops the non-ASCII rune ENTIRELY rather than half of it
    let sanitised = sanitizeSay(say)
    check sanitised.validateUtf8() == -1
    check sanitised.runeLen <= MaxSayRunes
    check '{' notin sanitised
    # and the record round-trips through a strict JSON parser
    var record = defaultStance()
    record.say = say
    record.note = "\u{1F3AF}".repeat(200)
    let text = boundedStanceRecord(record, 3, 1, 2)
    check text.runeLen <= MaxStanceRecordRunes
    check text.validateUtf8() == -1
    let parsed = parseJson(text)
    check parsed["k"].getStr == "stance"
    check parsed["say"].getStr.runeLen <= MaxSayRunes

  test "a reply with no valid stance object raises":
    expect StanceError:
      discard parse("""{"unrelated":true}""")
    expect StanceError:
      discard parse("""no json at all""")

  test "the emitted record carries both the readable float and the exact integers":
    var stance = defaultStance()
    stance.stance = stAim
    stance.aimAt = 1
    stance.targetBall = 0
    stance.postUu = -125_000
    stance.aggression255 = 204
    let node = stance.stanceRecordNode(7, 2, 3)
    check node["stance"].getStr == "aim"
    check node["aim_at"].getStr == "BLUE"
    check node["target_ball"].getStr == "B1"
    check node["post"].getFloat == -12.5
    check node["post_milli"].getInt == -12_500
    check node["aggression_255"].getInt == 204
    check node["alias"].getStr == "YELLOW"

  test "validateStance rejects exactly what the schema forbids":
    var stance = defaultStance()
    check stance.validateStance(2, NoneOut, BothLive) == ""
    stance.aimAt = 2
    check stance.validateStance(2, NoneOut, BothLive).len > 0
    stance.aimAt = 1
    var eliminated = NoneOut
    eliminated[1] = true
    check stance.validateStance(2, eliminated, BothLive).len > 0
    stance.aimAt = -1
    stance.targetBall = 1
    check stance.validateStance(2, NoneOut, @[true, false]).len > 0
    stance.targetBall = -1
    stance.postUu = PaddleTravelHalf + 1
    check stance.validateStance(2, NoneOut, BothLive).len > 0
