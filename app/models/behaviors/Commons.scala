package cards.models.behaviors

import cards.models.classes.{ Rank, Suit, SuitedCard, Card }
import cards.models.classes.Suit._
import cards.models.classes.Rank._

// Common card logic behaviors
trait Commons {

  def countRank(cards: Seq[Card]): Map[Rank, Int] = {
    (for (r <- Rank.values.toSeq) yield (r -> cards.count(_.rank == r))).toMap
  }

  def countSuit(cards: Seq[Card]): Map[Suit, Int] = {
    val suited: Seq[SuitedCard] = cards.filter(_.isInstanceOf[SuitedCard]).map(_.asInstanceOf[SuitedCard])
    (for (s <- Suit.values.toSeq) yield (s -> suited.count(_.suit == s))).toMap
  }

  // given cards, whether more than 1 card and each card is sequenced by rank (Two, Three, Four, etc... with no repeated ranks)
  def sequenced(cards: Seq[Card]): Boolean = cards.sorted.toList match {
    case Nil => false
    case x :: Nil => false
    case x :: xs => {
      val min: Int = x.rank.id
      val max: Int = xs.reverse.head.rank.id
      cards.sorted.map(_.rank.id).toSeq == (min to max).toSeq
    }
  }

  private object MinMax extends Enumeration {
    type MinMax = Value
    val Min, Max = Value
  }
  import MinMax._
  // rank of either lowest or highest card, optionally filtered by suit
  private def minMaxRank(cards: Seq[Card], minMax: MinMax, suit: Option[Suit] = None): Option[Rank] = (cards, minMax, suit) match {
    case (Nil, _, _) => None
    case (_, Min, Some(s)) => Some(cards.filter(c => c.isInstanceOf[SuitedCard] && c.asInstanceOf[SuitedCard].suit == s).min.rank)
    case (_, Max, Some(s)) => Some(cards.filter(c => c.isInstanceOf[SuitedCard] && c.asInstanceOf[SuitedCard].suit == s).max.rank)
    case (_, Min, None) => Some(cards.min.rank)
    case (_, Max, None) => Some(cards.max.rank)
    case (_, _, _) => None
  }

  // rank of lowest card, optionally filtered by suit
  def minRank(cards: Seq[Card], suit: Option[Suit] = None): Option[Rank] = minMaxRank(cards, Min, suit)

  // rank of highest card, optionally filtered by suit
  def maxRank(cards: Seq[Card], suit: Option[Suit] = None): Option[Rank] = minMaxRank(cards, Max, suit) 

  // lowest or highest by rank, optionally filtered by suit;
  // returns sequence to account for when there are multiples of the same rank 
  private def lowestOrHighest(cards: Seq[Card], minMax: MinMax, suit: Option[Suit] = None): Seq[Card] = minMaxRank(cards, minMax, suit) match {
    case None => Nil
    case Some(r) => cards.filter(_.rank == r) 
  }

  // lowest by rank, optionally filtered by suit; 
  // returns sequence to account for when there are multiples of the same rank 
  def lowest(cards: Seq[Card], suit: Option[Suit] = None): Seq[Card] = lowestOrHighest(cards, Min, suit) 

  // highest by rank, optionally filtered by suit; 
  // returns sequence to account for when there are multiples of the same rank 
  def highest(cards: Seq[Card], suit: Option[Suit] = None): Seq[Card] = lowestOrHighest(cards, Max, suit) 

}
