package cards.models.classes.actions

import play.api.libs.json.{ Json, Format }

object BlackjackAction extends Enumeration {
  type BlackjackAction = Value
  val Hit, Stand, Insurance, Split, DoubleDown = Value
  
  implicit val format: Format[BlackjackAction] = Json.formatEnum(this)
}

