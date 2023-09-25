package cards.classes.bettingstrategy

import play.api.libs.json.{ Json, Format }

object BlackjackBettingStrategy extends Enumeration {
  type BlackjackBettingStrategy = Value
  val Steady, Martingale, Oscars, PositiveProgression, NegativeProgression = Value
  implicit val format: Format[BlackjackBettingStrategy] = Json.formatEnum(this)
  def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
}
