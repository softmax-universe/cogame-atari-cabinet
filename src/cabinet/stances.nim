## The stance schema: what a cabinet's policy (LLM or scripted) may say, and how
## a canonical reply is strictly parsed and validated.
##
## Both policy kinds emit the SAME object, so the two are strictly comparable
## and one validator covers both — which is what makes the bounded-orders
## assertion in tests/test_baselines.nim meaningful.
##
## RUNE DISCIPLINE. Every cap here is measured in RUNES (Unicode codepoints)
## and every truncation lands on a rune boundary (`runeLen` / `runeSubStr`).
## Slicing a string by BYTE index anywhere on the path to the replay is
## forbidden: a byte-truncated multi-byte character renders fine in a browser
## and then fails a strict UTF-8 parser, which is exactly the class of bug
## that makes a replay unreadable to everything except the one viewer that
## happened to be lenient.

import std/[json, math, strutils, unicode]
import sim_types

type
  Stance* = enum
    ## A closed enum of canonical stance names.
    stGuard = "guard"
    stAim = "aim"
    stCamp = "camp"
    stCatch = "catch"
    stChase = "chase"

  StanceSource* = enum
    ssExternal = "external"
    ssScripted = "scripted"
    ssFallback = "fallback"

  CabinetStance* = object
    note*: string              ## <= MaxNoteRunes
    stance*: Stance
    targetBall*: int           ## ball index, or -1 for "any"
    aimAt*: int                ## cabinet index, or -1 for "none"
    postUu*: int32             ## along-units in µu, |post| <= PaddleTravelHalf
    leadTicks*: int            ## 0..48
    aggression255*: int        ## 0..255
    say*: string               ## <= MaxSayRunes, sanitized
    source*: StanceSource
    latencyMs*: int

  StanceError* = object of ValueError

const
  DefaultLeadTicks* = 12
  DefaultAggression255* = 204          ## 0.8
  MaxLeadTicks* = 48
  AllowedStanceKeys = [
    "stance", "target_ball", "aim_at", "post", "lead_ticks", "aggression",
    "note", "say"
  ]

proc truncateRunes*(text: string, limit: int): string =
  ## Cuts `text` to at most `limit` RUNES, on a rune boundary. The single
  ## place any recorded string is shortened.
  if limit <= 0:
    return ""
  if text.runeLen <= limit:
    return text
  text.runeSubStr(0, limit)

proc sanitizeSay*(text: string): string =
  ## Capped at MaxSayRunes on a rune boundary FIRST, then the starter's
  ## printable-ASCII shout filter. In that order the rune cut never leaves half
  ## a codepoint for the ASCII filter to smear. Braces are excluded
  ## deliberately: the replay chat stream tells a CONTROL record from a plain
  ## line by a leading '{'.
  result = ""
  for rune in text.truncateRunes(MaxSayRunes).runes:
    let value = int(rune)
    if value >= 32 and value < 127 and value != ord('{') and
        value != ord('}'):
      result.add($rune)
  result = result.strip()

proc sanitizeNote*(text: string): string =
  ## Newlines collapse to spaces so one record stays one line.
  text.replace("\n", " ").replace("\r", " ").strip().truncateRunes(MaxNoteRunes)

proc aggressionFraction*(stance: CabinetStance): float =
  float(stance.aggression255) / 255.0

proc postCu*(stance: CabinetStance): float =
  float(stance.postUu) / float(UuPerCu)

proc defaultStance*(): CabinetStance =
  CabinetStance(
    stance: stGuard, targetBall: -1, aimAt: -1, postUu: 0,
    leadTicks: DefaultLeadTicks, aggression255: DefaultAggression255,
    source: ssScripted)

proc holdFallbackStance*(): CabinetStance =
  ## Neutral guard-at-center fallback for failed/missing turns (never `bulwark`).
  CabinetStance(
    stance: stGuard,
    targetBall: -1,
    aimAt: -1,
    postUu: 0'i32,
    leadTicks: 0,
    aggression255: 0,
    source: ssFallback
  )

# ---------------------------------------------------------------------------
#  Canonical parsing
# ---------------------------------------------------------------------------

proc nativeReplyText*(body: string): string =
  ## The text actually received from the native Messages response.
  let payload = parseJson(body)
  if payload{"stop_reason"}.getStr() == "refusal":
    raise newException(StanceError, "anthropic refusal")
  for contentBlock in payload["content"]:
    if contentBlock{"type"}.getStr() == "text":
      result.add(contentBlock{"text"}.getStr())
  if payload{"stop_reason"}.getStr() == "max_tokens" and '{' notin result:
    raise newException(StanceError, "reply cut off at max_tokens before any JSON")

proc extractJsonObject*(text: string): JsonNode =
  ## Parses a raw JSON object or a single standard outer Markdown JSON fence
  ## (```json ... ``` or ``` ... ```). Rejects surrounding prose or non-object JSON.
  var body = text.strip()
  if body.startsWith("```"):
    let firstNl = body.find('\n')
    if firstNl < 0:
      raise newException(StanceError, "unterminated markdown fence")
    let header = body[3 ..< firstNl].strip()
    if header.len > 0 and header != "json":
      raise newException(StanceError, "unsupported markdown fence language: " & header)
    let rest = body[firstNl + 1 .. ^1]
    if not rest.endsWith("```"):
      raise newException(StanceError, "unterminated markdown fence")
    let inner = rest[0 ..< rest.len - 3]
    if inner.len == 0 or inner[^1] != '\n':
      raise newException(StanceError, "closing markdown fence must be on its own line")
    body = inner.strip()
  if body.len == 0 or body[0] != '{' or body[^1] != '}':
    var head = text.strip()
    if head.runeLen > 160:
      head = head.truncateRunes(160) & "..."
    raise newException(
      StanceError, "reply must be a JSON object: " & head.replace("\n", " "))
  let parsed =
    try:
      parseJson(body)
    except CatchableError as e:
      raise newException(StanceError, "invalid JSON object: " & e.msg)
  if parsed.isNil or parsed.kind != JObject:
    raise newException(StanceError, "reply is not a JSON object")
  parsed

proc parseStanceName*(text: string): tuple[ok: bool, stance: Stance] =
  ## Exact canonical stance enum only.
  for stance in Stance:
    if $stance == text:
      return (true, stance)
  (false, stGuard)

proc readFiniteNumber(node: JsonNode, field: string): float =
  if node.isNil or node.kind notin {JInt, JFloat}:
    raise newException(StanceError, field & " must be a number")
  result =
    if node.kind == JInt: float(node.getBiggestInt())
    else: node.getFloat()
  if classify(result) in {fcNan, fcInf, fcNegInf}:
    raise newException(StanceError, field & " must be finite")

proc parseTargetBall*(
  node: JsonNode, ballLive: openArray[bool]
): int =
  ## Canonical target_ball: "any" or "B1".."B<N>" naming a currently live ball.
  if node.isNil or node.kind != JString:
    raise newException(StanceError, "target_ball must be a string")
  let raw = node.getStr()
  if raw == "any":
    return -1
  if raw.len >= 2 and raw[0] == 'B' and raw[1] in {'1' .. '9'}:
    var allDigits = true
    for i in 1 ..< raw.len:
      if raw[i] notin {'0' .. '9'}:
        allDigits = false
        break
    if allDigits:
      var index = -1
      try:
        index = parseInt(raw[1 .. ^1]) - 1
      except CatchableError:
        index = -1
      if index >= 0 and index < ballLive.len and ballLive[index]:
        return index
  raise newException(StanceError, "invalid or non-live target_ball: " & raw)

proc parseAimAt*(
  node: JsonNode, myCabinet: int, cabinetOut: openArray[bool]
): int =
  ## Canonical aim_at: "none" or an alive rival's exact alias ("RED", "BLUE",
  ## "GREEN", "YELLOW"). Never self or an eliminated cabinet.
  if node.isNil or node.kind != JString:
    raise newException(StanceError, "aim_at must be a string")
  let raw = node.getStr()
  if raw == "none":
    return -1
  for i, alias in CabinetAliases:
    if raw == alias:
      if i == myCabinet:
        raise newException(StanceError, "aim_at cannot name own cabinet")
      if i < cabinetOut.len and cabinetOut[i]:
        raise newException(StanceError, "aim_at cannot name eliminated cabinet")
      return i
  raise newException(StanceError, "invalid aim_at: " & raw)

proc quantisePost*(value: float): int32 =
  ## Exact range -43.0..43.0 cu, quantised to µu. Out-of-range values are rejected.
  if classify(value) in {fcNan, fcInf, fcNegInf}:
    raise newException(StanceError, "post must be finite")
  let limit = float(PaddleTravelHalf) / float(UuPerCu)
  if value < -limit or value > limit:
    raise newException(StanceError, "post is outside -43.0..43.0")
  int32(round(value * float(UuPerCu)))

proc parseCabinetStance*(
  payload: JsonNode,
  myCabinet: int,
  cabinetOut: openArray[bool],
  ballLive: openArray[bool],
  catchEnabled: bool
): CabinetStance =
  ## Turns one parsed reply into a legal stance. Requires all six canonical
  ## action fields with exact names, types, and legal ranges; rejects unknown
  ## keys, aliases, synonyms, and illegal targets. Optional `note` and `say`
  ## are rune-capped.
  if payload.isNil or payload.kind != JObject:
    raise newException(StanceError, "reply is not a JSON object")
  for key in payload.keys:
    if key notin AllowedStanceKeys:
      raise newException(StanceError, "unknown stance field: " & key)

  result = defaultStance()
  result.source = ssExternal

  # optional note / say
  if payload.hasKey("note") and payload["note"].kind != JNull:
    if payload["note"].kind != JString:
      raise newException(StanceError, "note must be a string")
    result.note = sanitizeNote(payload["note"].getStr())
  if payload.hasKey("say") and payload["say"].kind != JNull:
    if payload["say"].kind != JString:
      raise newException(StanceError, "say must be a string")
    result.say = sanitizeSay(payload["say"].getStr())

  # stance
  if not payload.hasKey("stance") or payload["stance"].kind != JString:
    raise newException(StanceError, "stance must be a canonical string")
  let parsed = parseStanceName(payload["stance"].getStr())
  if not parsed.ok:
    raise newException(StanceError, "invalid stance: " & payload["stance"].getStr())
  if parsed.stance == stCatch and not catchEnabled:
    raise newException(StanceError, "catch is not enabled in this ROM")
  result.stance = parsed.stance

  # target_ball
  if not payload.hasKey("target_ball"):
    raise newException(StanceError, "missing target_ball")
  result.targetBall = parseTargetBall(payload["target_ball"], ballLive)

  # aim_at
  if not payload.hasKey("aim_at"):
    raise newException(StanceError, "missing aim_at")
  result.aimAt = parseAimAt(payload["aim_at"], myCabinet, cabinetOut)

  # post
  if not payload.hasKey("post"):
    raise newException(StanceError, "missing post")
  result.postUu = quantisePost(readFiniteNumber(payload["post"], "post"))

  # lead_ticks
  if not payload.hasKey("lead_ticks") or payload["lead_ticks"].kind != JInt:
    raise newException(StanceError, "lead_ticks must be an integer")
  let lead = payload["lead_ticks"].getBiggestInt()
  if lead < 0 or lead > MaxLeadTicks:
    raise newException(StanceError, "lead_ticks is outside 0..48")
  result.leadTicks = int(lead)

  # aggression
  if not payload.hasKey("aggression"):
    raise newException(StanceError, "missing aggression")
  let aggr = readFiniteNumber(payload["aggression"], "aggression")
  if aggr < 0.0 or aggr > 1.0:
    raise newException(StanceError, "aggression is outside 0.0..1.0")
  result.aggression255 = int(round(aggr * 255.0))

proc validateStance*(
  stance: CabinetStance,
  myCabinet: int,
  cabinetOut: openArray[bool],
  ballLive: openArray[bool]
): string =
  ## "" when the stance is legal, else the first violation. The scripted
  ## baselines are held to exactly this (tests/test_baselines.nim).
  if stance.note.runeLen > MaxNoteRunes:
    return "note exceeds " & $MaxNoteRunes & " runes"
  if stance.say.runeLen > MaxSayRunes:
    return "say exceeds " & $MaxSayRunes & " runes"
  if stance.postUu < -PaddleTravelHalf or stance.postUu > PaddleTravelHalf:
    return "post is outside the travel limit"
  if stance.leadTicks < 0 or stance.leadTicks > MaxLeadTicks:
    return "lead_ticks is outside 0..48"
  if stance.aggression255 < 0 or stance.aggression255 > 255:
    return "aggression is outside 0..255"
  if stance.targetBall >= 0:
    if stance.targetBall >= ballLive.len or not ballLive[stance.targetBall]:
      return "target_ball names a ball that is not live"
  if stance.aimAt >= 0:
    if stance.aimAt == myCabinet:
      return "aim_at names my own cabinet"
    if stance.aimAt >= cabinetOut.len or cabinetOut[stance.aimAt]:
      return "aim_at names an eliminated cabinet"
  ""

# ---------------------------------------------------------------------------
#  Records
# ---------------------------------------------------------------------------

proc stanceRecordNode*(
  stance: CabinetStance, turn, seat, cabinet: int
): JsonNode =
  ## The replay chat record for one turn's stance. Re-applied at playback into
  ## NON-HASHED sim fields only: it drives the scorebug stance chips, the aim
  ## rays, the match feed and tools/replay_summary.py, and can never affect
  ## the simulation.
  # The caps are re-applied HERE, at the record boundary, not only on parse: a
  # stance can also be built by a scripted baseline or by a tool, and EVERY
  # string that reaches the replay must be inside its cap on a RUNE boundary.
  %*{
    "k": "stance",
    "turn": turn,
    "seat": seat,
    "alias": aliasOfCabinet(cabinet),
    "cabinet": cabinet,
    "source": $stance.source,
    "latency_ms": stance.latencyMs,
    "note": stance.note.truncateRunes(MaxNoteRunes),
    "stance": $stance.stance,
    "target_ball":
      (if stance.targetBall < 0: "any" else: "B" & $(stance.targetBall + 1)),
    "aim_at":
      (if stance.aimAt < 0: "none" else: aliasOfCabinet(stance.aimAt)),
    "post": stance.postCu(),
    "post_milli": int((int64(stance.postUu) * 1000'i64) div int64(UuPerCu)),
    "lead_ticks": stance.leadTicks,
    "aggression": stance.aggressionFraction(),
    "aggression_255": stance.aggression255,
    "say": stance.say.truncateRunes(MaxSayRunes)
  }

proc boundedStanceRecord*(
  stance: CabinetStance, turn, seat, cabinet: int
): string =
  ## The serialized stance record, guaranteed <= MaxStanceRecordRunes. The
  ## note is the only unbounded-in-practice field, so it is the one that
  ## shrinks, and the cut still lands on a RUNE boundary. Never cut the
  ## SERIALIZED string — that would emit broken JSON, which is the exact
  ## failure the rune rule exists to prevent.
  var trimmed = stance
  result = $trimmed.stanceRecordNode(turn, seat, cabinet)
  var guard = 0
  while result.runeLen > MaxStanceRecordRunes and guard < 12:
    inc guard
    let keep = max(0, trimmed.note.runeLen - max(8, trimmed.note.runeLen div 2))
    trimmed.note = trimmed.note.truncateRunes(keep)
    trimmed.say = trimmed.say.truncateRunes(max(0, trimmed.say.runeLen - 2))
    result = $trimmed.stanceRecordNode(turn, seat, cabinet)

proc stanceActionNode*(stance: CabinetStance): JsonNode =
  ## Only policy-controlled semantic fields; runtime/replay metadata stays separate.
  result = stance.stanceRecordNode(0, 0, 0)
  for field in ["k", "turn", "seat", "alias", "cabinet", "source", "latency_ms", "post_milli", "aggression_255"]:
    result.delete(field)
