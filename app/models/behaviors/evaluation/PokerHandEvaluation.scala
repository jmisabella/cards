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

    def evaluateCard(card: Card): Int = card.rank.id + 2 // first Rank id is 0 which belongs to Rank Two, so add 2 to the Rank's id

    def evaluateCards(cards: Seq[Card]): Int = cards.foldLeft(0)((acc, c) => acc + evaluateCard(c))

    predicates.handType(cards) match {
      // multiply matched card score by ten-to-the-matched-PokerHandType (id+2) exponent, then add unmatched cards' score
      case Some(t) => (pow(10, t.id + 2).toInt * evaluateCards(predicates.matched(cards))) + evaluateCards(predicates.unmatched(cards))
      case None => evaluateCards(cards)
    }
  }
}