package cards.models.classes.actions

import play.api.libs.json.{ Json, Format }

object ThirtyOneAction extends Enumeration {
  type ThirtyOneAction = Value
  val Draw, Discard, DrawFromDiscard, Knock = Value
  
  implicit val format: Format[ThirtyOneAction] = Json.formatEnum(this)
}
