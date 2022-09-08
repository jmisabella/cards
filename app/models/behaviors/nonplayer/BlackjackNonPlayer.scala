package cards.models.behaviors.nonplayer

import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.behaviors.betting.BlackjackBetting
import cards.models.behaviors.play.BlackjackPlay
import cards.models.classes.state.{ BlackjackPlayerState, BlackjackGameState }
import cards.models.classes.{ Card, Deck }
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._

trait BlackjackNonPlayer { 
  type EVAL <: BlackjackHandEvaluation 
  type PREDICATES <: BlackjackPredicates
  type BETTING <: BlackjackBetting
  type PLAY <: BlackjackPlay
  val evaluation: EVAL
  val predicates: PREDICATES
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

    ???
  }

}
