package cards.behaviors.evaluation

import cards.classes.Card
import cards.classes.Rank._
import cards.behaviors.predicates.PokerPredicates
import scala.annotation.tailrec
import scala.math.pow

trait PokerHandEvaluation extends HandEvaluation {
  type P <: PokerPredicates 
  val predicates: P
  
  override def eval(cards: Seq[Card]): Long = {
    // cards in poker hand are valued by the same order defined for our Rank enumeration
    def evaluateCard(card: Card): Int = card.rank.id + 2 // first Rank id is 0 which belongs to Rank Two, so add 2 to the Rank's id to make them match numbers

    def evaluateCards(cards: Seq[Card]): Long = cards.foldLeft(0)((acc, c) => acc + evaluateCard(c))

    predicates.handType(cards) match {
      // We need to add weight by how good the poker hand is based on PokerHandType, then add the unmatched cards' score as an unweighted value to settle any ties.
      // In order to do this, multiply matched cards' score by ten-to-the-matched-PokerHandType (id+2) exponent, then add unmatched cards' score.
      case Some(t) => (pow(10, t.id + 2).toInt * evaluateCards(predicates.matched(cards))) + evaluateCards(predicates.unmatched(cards))
      case None => evaluateCards(cards)
    }
  }
}