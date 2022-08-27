package cards.models.behaviors.nonplayer

import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.classes.state.{ BlackjackPlayerState, BlackjackGameState }
import cards.models.classes.{ Card, Deck }
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._

trait BlackjackNonPlayer { 
  type EVAL <: BlackjackHandEvaluation 
  type PREDICATES <: BlackjackPredicates
  val evaluation: EVAL
  val predicates: PREDICATES

  def next(gameState: BlackjackGameState): BlackjackGameState = {
    if (gameState.deck.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because deck is empty")
    }
    if (gameState.players.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because there are no players")
    }
    // TODO: test 
    if (gameState.isTimeToSettle()) {
      return gameState.settleBets()
    }
    // TODO: need game state to have knowledge of current player's current hand (to iterate through hands)...
    ???
  }

}
