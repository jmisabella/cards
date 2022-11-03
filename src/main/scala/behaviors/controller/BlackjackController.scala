package cards.behaviors.controller

import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.behaviors.betting.BlackjackBetting
import cards.behaviors.play.BlackjackPlay
import cards.classes.state.{ BlackjackPlayerState, BlackjackGameState }
import cards.classes.{ Card, Deck }
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._

trait BlackjackController { 
  type BETTING <: BlackjackBetting
  type PLAY <: BlackjackPlay
  val betting: BETTING
  val play: PLAY

  def next(game: BlackjackGameState): BlackjackGameState = {
    if (game.deck.length == 0) {
      throw new IllegalStateException(
        s"Cannot proceed to next state because deck is empty")
    }
    if (game.players.length == 0) {
      throw new IllegalStateException(
        s"Cannot proceed to next state because there are no players")
    }
    if (game.currentPlayerIndex.isEmpty && (betting.isTimeToPlaceNewBets(game) || betting.isTimeToSettle(game))) {
      // throw new IllegalStateException(
      //   "Cannot proceed to next state because no player is designated as the current player")
      return game.copy(currentPlayerIndex = Some(0), currentHandIndex = Some(0))
    }
    if (betting.isTimeToSettle(game)) {
      // IMPORTANT: // TODO: after bets are settled, how do we get from history showing Wins, Losses, Ties back to an empty history ?????
      println("SETTLING")
      return betting.settleBets(game)
    }
    if (betting.isTimeToPlaceNewBets(game)) {
      println("BETTING")
      val adjustedStrategy: BlackjackGameState = betting.alterBettingStrategy(game.currentPlayer(), game) 
      val adjustedBetting: BlackjackGameState = betting.alterMinBet(adjustedStrategy.currentPlayer(), adjustedStrategy)
      return betting.placeBet(adjustedBetting) // TODO: test
    }
    // if (play.isTimeToPlay(game)) {
    //   return play.playHand(game) // TODO: test
    // }
    // if (play.isTimeToDeal(game)) {
    //   return play.deal(game) // TODO: test
    // }
    // if (play.isTimeForDealerToPlay(game)) {
    //   return play.dealerPlay(game) // TODO: test
    // }
    val nextState: BlackjackGameState = 


      if (play.isTimeForDealerToPlay(game)) {
        println("DEALER PLAYING")
        play.dealerPlay(game)
      } else if (play.isTimeToDeal(game)) {
        println("DEALING")
        play.deal(game)
      } else if (play.isTimeToPlay(game)) {
        println("PLAYING")
        play.playHand(game)
      // } else if (play.isTimeForDealerToPlay(game)) {
      //   play.dealerPlay(game)
      } else {
        game
      }
    
    // val test1 = game.currentPlayerIndex.isDefined
    // val test2 = game.currentHandIndex.isDefined
    // val test3 = play.isTimeToPlay(game)
    // val test4 = play.isTimeToDeal(game)
    // val test5 = nextState.currentHand() // this errors
    // val test6 = play.evaluation.eval(nextState.currentHand().hand) > 21

    // if (game.currentPlayerIndex != None && game.currentHandIndex != None && (play.isTimeToPlay(game) || play.isTimeToDeal(game))) {
    //   // iterate to next hand
    //   // return nextState.toNextHand(nextState.history.reverse.head.action, play.evaluation.eval(nextState.currentHand().hand) > 21)
    //   // val busted = try { play.evaluation.eval(nextState.currentHand().hand) > 21 } catch { case _: Exception => true }
    //   val busted = try { play.evaluation.eval(nextState.currentHand().hand) > 21 } catch { case _: Exception => false }
    //   return nextState//.toNextHand(nextState.history.reverse.head.action, busted)
    // } else {
    //   return nextState
    // }
    return nextState

    // TODO: do player rotations all work correctly?
    // TODO: after dealer plays, should we set a flag indicating game is completed?


    ???
  }

  def init(playerNames: Seq[String], tokens: Int): BlackjackGameState = {
    val players: Seq[BlackjackPlayerState] = for (player <- playerNames) yield BlackjackPlayerState(s"$player", tokens)
    BlackjackGameState(players = players)
  }

  def init(playerCount: Int = 1, tokens: Int = 1000): BlackjackGameState = {
    val players: Seq[String] = for (i <- 0 to playerCount) yield s"player${i+1}"
    init(players, tokens)
  }
}
