import std/[json, unittest]
import cabinet/[sim, stances, control, baselines, decide, server]
import helpers

proc playingGame(config: GameConfig): SimServer =
  result = initSimServer(config)
  result.gameEventLoggingEnabled = false
  for seat in 0 ..< CabinetCount:
    discard result.addPlayer("P" & $(seat + 1), seat, "token-" & $seat)
  result.phase = Playing
  result.gameStartTick = 0

suite "ordinary player orders":
  test "complete orders are applied, malformed and stale orders are rejected":
    let game = playingGame(episodeConfig(7))
    var engine = initDecisionEngine(game)
    let action = %*{"stance": "camp", "post": 17, "lead_ticks": 3,
      "aggression": 0.2, "target_ball": "any", "aim_at": "none",
      "note": "private decision", "say": "ready"}
    check not engine.acceptOrders(game, 0, 2, "not JSON")
    check not engine.acceptOrders(game, 0, 2, $(%*{"type": "orders", "turn": 1, "action": action}))
    check not engine.haveStance[0]
    check engine.acceptOrders(game, 0, 2, $(%*{"type": "orders", "turn": 2, "action": action}))
    check engine.haveStance[0]
    check engine.stances[0].stance == stCamp
    check engine.stances[0].postUu == 17 * UuPerCu
    check engine.stances[0].leadTicks == 3
    check engine.stances[0].source == ssExternal
    check not engine.haveStance[1]

  test "minimal stance-only orders are accepted with fixed defaults (never inherited)":
    let game = playingGame(episodeConfig(8))
    var engine = initDecisionEngine(game)
    let turn0 = %*{"type": "orders", "turn": 0, "action": {
      "stance": "camp", "post": 20.0, "lead_ticks": 4, "aggression": 0.1
    }}
    check engine.acceptOrders(game, 0, 0, $turn0)
    check engine.stances[0].postUu == 20 * UuPerCu
    check engine.stances[0].leadTicks == 4
    let turn1 = %*{"type": "orders", "turn": 1, "action": {"stance": "guard"}}
    check engine.acceptOrders(game, 0, 1, $turn1)
    check engine.stances[0].stance == stGuard
    check engine.stances[0].postUu == 0
    check engine.stances[0].leadTicks == DefaultLeadTicks
    check engine.stances[0].aggression255 == DefaultAggression255

  test "missed/failed turns clear stance (no synthesized stance or scripted bulwark)":
    let game = playingGame(episodeConfig(19))
    var engine = initDecisionEngine(game)
    check engine.acceptOrders(game, 1, 0, $(%*{"type": "orders", "turn": 0, "action": {"stance": "guard"}}))
    check engine.haveStance[1]
    engine.clearStance(1)
    check not engine.haveStance[1]
    check not engine.acceptOrders(game, 1, 1, $(%*{"type": "orders", "turn": 1, "source": "fallback", "action": {"stance": "guard"}}))
    check not engine.haveStance[1]

  test "the wall-clock stop yields deadline/wall_clock and still scores the board":
    let config = episodeConfig(10)
    var game = playingGame(config)
    var commands = newSeq[uint8](CabinetCount)
    for seat in 0 ..< CabinetCount:
      commands[seat] = NeutralCommand
    for tick in 0 ..< 300:
      game.step(commands)
    game.stopForWallClock()
    check game.phase == GameOver
    check game.endReason == ReasonDeadline
    check game.endRule == EndRuleWallClock
    check game.winnerCabinet >= 0
    var crowns = 0
    for k in 0 ..< CabinetCount:
      if game.cabinets[k].placement == 1:
        inc crowns
    check crowns == 1

  test "a tripped invariant yields fault/sim_fault":
    let config = episodeConfig(11)
    var game = playingGame(config)
    game.faultGame(EndRuleSimFault)
    check game.phase == GameOver
    check game.endReason == ReasonFault
    check game.endRule == EndRuleSimFault
    let document = parseJson(game.playerResultsJson())
    check document["reason"].getStr == ReasonFault
    check document["endRule"].getStr == EndRuleSimFault
    check document["scores"].len == CabinetCount

  test "illegal field values are rejected by acceptOrders instead of repaired":
    let config = episodeConfig(14)
    var game = playingGame(config)
    var engine = initDecisionEngine(game)
    let cabinet = game.cabinetOfSeat(0)
    game.balls[1].state = bsServing            ## B2 is not live any more
    let badBall = %*{"type": "orders", "turn": 0, "action": {
      "stance": "camp", "target_ball": "B2", "aim_at": "none",
      "post": 0.0, "lead_ticks": 12, "aggression": 0.5
    }}
    check not engine.acceptOrders(game, 0, 0, $badBall)
    check not engine.haveStance[0]

    let badRange = %*{"type": "orders", "turn": 0, "action": {
      "stance": "aim", "target_ball": "any", "aim_at": aliasOfCabinet(cabinet),
      "post": 80.0, "lead_ticks": 900, "aggression": -0.5
    }}
    check not engine.acceptOrders(game, 0, 0, $badRange)
    check not engine.haveStance[0]

    let badSynonym = %*{"type": "orders", "turn": 0, "action": {
      "stance": "defend", "target_ball": "any", "aim_at": "none",
      "post": 0.0, "lead_ticks": 12, "aggression": 0.8
    }}
    check not engine.acceptOrders(game, 0, 0, $badSynonym)
    check not engine.haveStance[0]

  test "parseRegistration reads the seat's ONE message and drops anything else":
    check parseRegistration("""{"type":"register","kind":"prompt","policy":"x"}""").ok
    check parseRegistration(
      """{"type":"register","kind":"scripted","scripted":"spinner"}""").scripted ==
      "spinner"
    check not parseRegistration("""{"type":"chat","text":"hi"}""").ok
    check not parseRegistration("hello").ok
    check not parseRegistration("").ok
    check parseBaseline("spinner") == blSpinner
    check parseBaseline("bulwark") == blBulwark
    check parseBaseline("") == blBulwark
    check parseBaseline("nonsense") == blBulwark
