package cards.models.behaviors.evaluation

import cards.models.classes.Card
import cards.models.classes.Rank._
import cards.models.behaviors.predicates.PokerPredicates
import scala.annotation.tailrec
import scala.math.pow

trait PokerHandEvaluation extends HandEvaluation {
  type P <: PokerPredicates 
  val predicates: P
  
  override def eval(cards: Seq[Card]): Int = {

    def evaluateCard(card: Card): Int = card.rank match {
      case Jack => 11
      case Queen => 12
      case King => 13
      case Ace => 14
      case r => predicates.commons.getNumeric(r)
    }

    def evaluateCards(cards: Seq[Card]): Int = cards.foldLeft(0)((acc, c) => acc + evaluateCard(c))

    predicates.handType(cards) match {
      case Some(t) => (pow(10, t.id + 2).toInt * evaluateCards(predicates.matched(cards))) + evaluateCards(predicates.unmatched(cards))
      case None => evaluateCards(cards)
    }
  }

}