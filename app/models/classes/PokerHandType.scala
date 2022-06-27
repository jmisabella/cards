package cards.models.classes

object PokerHandType extends Enumeration {
  type PokerHandType = Value
  val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush = Value
}