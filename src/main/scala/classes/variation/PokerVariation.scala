package classes.variation

import play.api.libs.json.{ Json, Format }

object PokerVariation extends Enumeration {
  type PokerVariation = Value
  val Straight, Stud, Draw, CommunityCard = Value
  implicit val format: Format[PokerVariation] = Json.formatEnum(this)
}
