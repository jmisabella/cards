package cards.models.classes

import play.api.libs.json.{ Json, Format }

object PokerHandCategorization extends Enumeration {
  type PokerHandCategorization = Value
  val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush = Value

  implicit val format: Format[PokerHandCategorization] = Json.formatEnum(this)
}