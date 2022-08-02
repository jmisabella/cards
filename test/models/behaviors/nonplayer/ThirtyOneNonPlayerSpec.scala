package cards.models.behaviors.nonplayer

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.state.{ ThirtyOneGameState, ThirtyOnePlayerState }
import org.scalatest.flatspec.AnyFlatSpec

class ThirtyOneNonPlayerSpec extends AnyFlatSpec {
  private[nonplayer] case object _commons extends Commons
  private [nonplayer] case object _evaluation extends ThirtyOneHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  case object module extends ThirtyOneNonPlayer {
    override type EVAL = ThirtyOneHandEvaluation
    override val evaluation = _evaluation
  }

  "ThirtyOneNonPlayer" should "throw IllegalStateException when calling next() from game state which does not yet have any player states" in {
    try {
      val gameState = ThirtyOneGameState(Nil)
      val result = module.next(gameState) 
      assert(false)
    } catch {
      case e: IllegalStateException => assert(true)
    }
  }
}
