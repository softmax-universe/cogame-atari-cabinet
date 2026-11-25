## Ordinary cabinet player: complete standing orders over private seat views.
## PLAYER_PROMPT and inference credentials belong to this process only.
import std/[httpclient, json, monotimes, options, os, strutils, times, uri]
import bitworld/spriteprotocol
import whisky
import curly
import cabinet/[sim_types, stances, baselines, player_baselines, player_llm, numeric_codec, prompts, capture]

const
  ConnectAttempts = 240      ## 240 x 500 ms = 2 minutes of dialling.
  ConnectRetryMs = 500
  RegistrationResends = 10   ## re-sends after the first, ~1 s apart.
  ResendEveryFrames = 24     ## ~1 s of frames at 24 Hz.
  ReconnectAttempts = 6

proc registrationBlob(kind, scripted, policy: string): string =
  ## Policy attribution only. Prompts remain local.
  var node = %*{
    "type": "register",
    "kind": kind,
    "policy": policy
  }
  if scripted.len > 0:
    node["scripted"] = %scripted
  else:
    node["scripted"] = newJNull()
  blobFromSpriteChat($node)

proc readyBlob(): string =
  ## Acknowledge binary viewer frames; standing orders use the Text channel.
  result = newString(1)
  result[0] = char(0x85)

when isMainModule:
  let url = getEnv("COWORLD_PLAYER_WS_URL", getEnv("COGAMES_ENGINE_WS_URL"))
  if url.len == 0:
    quit("COWORLD_PLAYER_WS_URL is not set", 1)
  let
    numericUrl = getEnv("PLAYER_NUMERIC_URL").strip()
    prompt = getEnv("PLAYER_PROMPT").strip()
    scripted = getEnv("PLAYER_SCRIPTED").strip()
    endpoint = getEnv("COWORLD_LLM_ENDPOINT").strip()
    llmEnabled = getEnv("COWORLD_LLM_ENABLED").strip().toLowerAscii() in
      ["1", "true", "yes"]
    kind =
      if numericUrl.len > 0: "external"
      elif scripted.len > 0: "scripted"
      elif llmEnabled or prompt.len > 0 or endpoint.len > 0: "prompt"
      else:
        quit("cabinet player: no policy configured (set PLAYER_SCRIPTED, PLAYER_NUMERIC_URL, or COWORLD_LLM_ENABLED/PLAYER_PROMPT)", 1)
    scriptedBaseline =
      if kind == "scripted":
        case scripted.toLowerAscii()
        of "bulwark": blBulwark
        of "spinner": blSpinner
        else:
          quit("cabinet player: unknown PLAYER_SCRIPTED baseline: " & scripted, 1)
      else:
        blBulwark
    label = block:
      let explicit = getEnv("PLAYER_POLICY_LABEL").strip()
      if explicit.len > 0: explicit
      elif numericUrl.len > 0: "numeric"
      elif kind == "prompt": "prompt"
      else: $scriptedBaseline
  let client =
    if kind == "prompt":
      newLlmClient(getEnv("PLAYER_MODEL", "claude-haiku-4-5-20251001"),
        parseInt(getEnv("PLAYER_MAX_OUTPUT_TOKENS", "900")))
    else: nil

  doAssert numericUrl.len == 0 or prompt.len == 0
  let numericClient = newHttpClient(timeout = 5000)
  numericClient.headers = newHttpHeaders({"Content-Type": "application/json"})
  let numericKey = getEnv("PLAYER_NUMERIC_KEY")
  if numericKey.len > 0: numericClient.headers["Authorization"] = "Bearer " & numericKey
  var seat = -1
  for key, value in decodeQuery(parseUri(url).query):
    if key == "slot": seat = parseInt(value)
  doAssert seat >= 0 and seat < CabinetCount
  let session = getEnv("PLAYER_POLICY_SESSION", "cabinet-" & $getCurrentProcessId())

  var socket: WebSocket

  proc orders(decision: JsonNode): string =
    let view = decision["view"]
    if numericUrl.len > 0:
      let encoding = numericEncoding(view, decision["turn"].getInt())
      let request = %*{"session": session, "seat": seat,
        "decision_id": encoding["decision_id"], "values": encoding["values"],
        "action_mask": encoding.actionMask()}
      let response = parseJson(numericClient.postContent(numericUrl, $request))
      let action = decodeActions(view, response["actions"])
      return $(%*{"type": "orders", "turn": decision["turn"], "action": action})
    var cabinet = -1
    var cabinetOut: array[CabinetCount, bool]
    for index, alias in CabinetAliases:
      if alias == view["you"]["alias"].getStr():
        cabinet = index
        cabinetOut[index] = view["you"]["out"].getBool()
      for rival in view["rivals"]:
        if rival["alias"].getStr() == alias:
          cabinetOut[index] = rival["out"].getBool()
    doAssert cabinet >= 0
    var live: seq[bool]
    for ball in view["balls"]:
      live.add(ball["state"].getStr() == "live")
    var evidence = newDecisionAttempt($decision["turn"].getInt() & "-" & $seat, label, aoUnknown)
    if kind == "prompt":
      if client.disabled:
        let reason = "LLM provider is unavailable"
        evidence.rejectionReason = some(reason)
        return $(%*{"type": "orders_failed", "turn": decision["turn"],
          "kind": "episode_inconclusive", "reason": reason, "training_attempt": attemptEvidenceJson(evidence)})
      client.throttled = false
      # The player performs inference. All seats run concurrently in their
      # own containers; the game owns their shared response deadline.
      let system = decision["system"].getStr()
      let user = userMessage(prompt, $view)
      let request = client.requestFor(system, user, seat)
      evidence = requestEvidence(evidence.attemptId, label, system, user, request.body)
      socket.send($( %*{"type": "attempt_started", "turn": decision["turn"],
        "training_attempt": attemptEvidenceJson(evidence)}), TextMessage)
      let started = getMonoTime()
      var batch: RequestBatch
      batch.post(request.url, request.headers, request.body, "player")
      let responses = client.curl.makeRequests(batch,
        max(1, (decision["deadline_ms"].getInt() - 500) div 1000))
      evidence.latencyMs = some(float((getMonoTime() - started).inMilliseconds))
      evidence.receivedEvidence(responses[0].response)
      socket.send($( %*{"type": "attempt_response", "turn": decision["turn"],
        "training_attempt": attemptEvidenceJson(evidence)}), TextMessage)
      var failureKind = "episode_inconclusive"
      try:
        let text = client.textOf(responses[0].response, responses[0].error, request.url)
        failureKind = "player_error"
        evidence.response = %text
        let stance = parseCabinetStance(extractJsonObject(text), cabinet,
          cabinetOut, live, view["rules"]["catch_enabled"].getBool())
        let action = stance.stanceActionNode()
        return $(%*{"type": "orders", "turn": decision["turn"], "action": action,
          "source": "model", "training_attempt": attemptEvidenceJson(evidence)})
      except CatchableError as error:
        echo "cabinet player: provider/parse failed: ", error.msg
        evidence.rejectionReason = some(error.msg)
        return $(%*{"type": "orders_failed", "turn": decision["turn"],
          "kind": failureKind, "reason": error.msg, "training_attempt": attemptEvidenceJson(evidence)})
    let stance = view.baselineStance(scriptedBaseline)
    let action = stance.stanceActionNode()
    $(%*{"type": "orders", "turn": decision["turn"], "action": action,
      "source": "scripted", "training_attempt": attemptEvidenceJson(evidence)})

  echo "cabinet player: kind=",
    (if kind == "prompt": "llm" else: kind),
    " baseline=", (if kind == "scripted": $scriptedBaseline else: "none"),
    " label=", label

  proc dial(attempts: int): WebSocket =
    ## Bounded dialling. The game bakes its board render caches BEFORE it opens
    ## the listener, and the episode runner starts the players at the same
    ## instant as the game — so the first dial always lands on a closed port.
    for attempt in 0 ..< attempts:
      try:
        return newWebSocket(url)
      except CatchableError as error:
        if attempt == 0:
          echo "cabinet player: game not listening yet (", error.msg,
            "); retrying"
        sleep(ConnectRetryMs)
    nil

  socket = dial(ConnectAttempts)
  if socket == nil:
    quit("cabinet player: game never accepted a connection", 1)
  echo "cabinet player: connected"

  # Each session is wrapped: whisky's receiveMessage RAISES on a close frame or
  # a truncated read (only a timeout returns none), and mummy's send only
  # QUEUES — so the game's own quit(0) can outrun the flushed frame. A naive
  # player exits 1 on that race and fails certification intermittently
  # (cogame-raid 0.1.3). EXITING 0 ON A DEAD SOCKET IS THE FIX.
  #
  # REGISTRATION IS RE-SENT, NOT SENT ONCE. Joins are slot-sequential, so a
  # seat whose slot is not the next open one is not admitted until the lower
  # slots have joined — and the lobby sends frames to a socket before it is
  # admitted, so the first registration AND a single re-send keyed on the first
  # received frame can both land while the seat has no index yet. The server
  # holds an unappliable registration, and this end keeps re-sending it for the
  # first ~10 s of frames. Registering twice is harmless.
  var reconnects = 0
  while true:
    var sessionFrames = 0
    socket.send(registrationBlob(kind, scripted, label), BinaryMessage)
    var resends = 0
    while true:
      var received: Option[Message]
      try:
        received = socket.receiveMessage()
      except CatchableError as error:
        echo "cabinet player: socket closed (", error.msg, ")"
        break
      if received.isNone:
        continue
      if received.get().kind == TextMessage:
        let decision = parseJson(received.get().data)
        if decision["type"].getStr() == "decision":
          doAssert decision["protocol"].getStr() == "atari-cabinet.player.v2"
          socket.send(orders(decision), TextMessage)
        continue
      inc sessionFrames
      if resends < RegistrationResends and
          sessionFrames mod ResendEveryFrames == 1:
        inc resends
        socket.send(registrationBlob(kind, scripted, label), BinaryMessage)
      socket.send(readyBlob(), BinaryMessage)
    if sessionFrames == 0 or reconnects >= ReconnectAttempts:
      break
    inc reconnects
    echo "cabinet player: re-dialling the seat (attempt ", reconnects, ")"
    socket = dial(ReconnectAttempts)
    if socket == nil:
      echo "cabinet player: game is no longer listening, exiting cleanly"
      break
    echo "cabinet player: reconnected, re-registering"
  numericClient.close()
  quit(0)
