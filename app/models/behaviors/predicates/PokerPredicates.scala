package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._

trait PokerPredicates {
  type CB <: Commons
  val commons: CB 

  def straight(cards: Seq[Card]): Boolean = commons.sequenced(cards)

  def flush(cards: Seq[Card]): Boolean = commons.countSuit(cards).values.toSeq.contains(cards.length)

  def straightFlush(cards: Seq[Card]): Boolean = straight(cards) && flush(cards)

  def royalFlush(cards: Seq[Card]): Boolean = straightFlush(cards) && cards.count(c => Seq(Ten, Jack, Queen, King, Ace).contains(c)) == cards.length

  def onePair(cards: Seq[Card]): Boolean = commons.countRank(cards).values.toSeq.count(_ == 2) == 1
  
  def twoPair(cards: Seq[Card]): Boolean = commons.countRank(cards).values.toSeq.count(_ == 2) == 2

  def threeOfAKind(cards: Seq[Card]): Boolean = commons.countRank(cards).values.toSeq.contains(3)
  
  def fourOfAKind(cards: Seq[Card]): Boolean = commons.countRank(cards).values.toSeq.contains(4)

  def fullHouse(cards: Seq[Card]): Boolean = onePair(cards) && threeOfAKind(cards)

  def highCard(cards: Seq[Card]): Boolean = !onePair(cards) && !twoPair(cards) && !straight(cards) && !flush(cards) && !threeOfAKind(cards) && !fourOfAKind(cards)

}