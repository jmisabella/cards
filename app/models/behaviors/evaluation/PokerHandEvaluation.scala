package cards.models.behaviors.evaluation

import cards.models.classes.{ Card, SuitedCard, PokerHandType }
import cards.models.classes.Rank._
import cards.models.classes.PokerHandType._
import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.PokerPredicates
import scala.annotation.tailrec
import scala.math.pow

trait PokerHandEvaluation extends HandEvaluation {
  type P <: PokerPredicates 
  val predicates: P
  
  override def eval(cards: Seq[Card]): Int = {
    def scoreBase(exponent: Int): Int = pow(10, exponent).toInt

    def cardEval(card: Card): Int = card.rank match {
      case Jack => 11
      case Queen => 12
      case King => 13
      case Ace => 14
      case r => predicates.commons.getNumeric(r)
    }

    def handEval(cards: Seq[Card]): Int = cards.foldLeft(0)((acc, c) => acc + cardEval(c))

    predicates.handType(cards) match {
      case Some(t) => (scoreBase(t.id + 1) * handEval(predicates.matched(cards))) + handEval(predicates.unmatched(cards))
      case None => handEval(cards)
    }
  }
}