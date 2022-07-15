package cards.models.behaviors.nonplayer

import cards.models.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.models.classes.state.{ ThirtyOnePlayerState, ThirtyOneGameState }
import cards.models.classes.{ Card, Deck }
import cards.models.classes.actions.{ Action, ThirtyOneAction }

trait ThirtyOneNonPlayer {
  type EVAL <: ThirtyOneHandEvaluation 
  val evaluation: EVAL
  
  // TODO: implement

  def next[A <: Enumeration#Value](playerState: ThirtyOnePlayerState, gameState: ThirtyOneGameState): ThirtyOneGameState = {
    ???
  }

}
