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
    if (game.currentPlayerIndex.isEmpty) {
      throw new IllegalStateException(
        "Cannot proceed to next state because no player is designated as the current player")
    }
    if (betting.isTimeToSettle(game)) {
      return betting.settleBets(game)
    }
    if (betting.isTimeToPlaceNewBets(game)) {
      val adjustedStrategy: BlackjackGameState = betting.alterBettingStrategy(game.currentPlayer(), game) 
      val adjustedBetting: BlackjackGameState = betting.alterMinBet(adjustedStrategy.currentPlayer(), adjustedStrategy)
      return betting.placeBet(adjustedBetting) // TODO: test
    }
    if (play.isTimeToPlay(game)) {
      return play.playHand(game) // TODO: test
    }
    if (play.isTimeToDeal(game)) {
      return play.deal(game) // TODO: test
    }
    if (play.isTimeForDealerToPlay(game)) {
      return play.dealerPlay(game) // TODO: test
    }
    // TODO: do player rotations all work correctly?
    // TODO: after dealer plays, should we set a flag indicating game is completed?


    ???
  }

}
