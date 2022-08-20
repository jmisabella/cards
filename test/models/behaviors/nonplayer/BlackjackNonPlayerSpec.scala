package cards.models.behaviors.nonplayer

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.behaviors.nonplayer.BlackjackNonPlayer
import cards.models.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackNonPlayerSpec extends AnyFlatSpec with GivenWhenThen {
  private [nonplayer] case object _commons extends Commons
  private [nonplayer] case object _evaluation extends BlackjackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  private [nonplayer] case object _predicates extends BlackjackPredicates {
    override type CB = Commons
    override val commons = _commons
  }
  case object module extends BlackjackNonPlayer {
    override type EVAL = BlackjackHandEvaluation
    override type PREDICATES = BlackjackPredicates
    override val evaluation = _evaluation
    override val predicates = _predicates
  }

  "BlackjackNonPlayer" should "stand on a non-splittable 20" in {
    
    pending
  }
}