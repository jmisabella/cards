package cards.classes

import play.api.libs.json.{ Json, Format }

object Outcome extends Enumeration {
  type Outcome = Value
  val Win, Lose, Tie = Value
  
  implicit def ordering [A <: Outcome]: Ordering[A] = Ordering.by(_.id)
  implicit val format: Format[Outcome] = Json.formatEnum(this)
}