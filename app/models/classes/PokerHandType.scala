package cards.models.classes

import play.api.libs.json.{ Json, Format }

object PokerHandType extends Enumeration {
  type PokerHandType = Value
  val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush = Value

  implicit val format: Format[PokerHandType] = Json.formatEnum(this)
}