package cards.classes.actions

import play.api.libs.json.{ Json, Format }

object ThirtyOneAction extends Enumeration {
  type ThirtyOneAction = Value
  val DrawFromStock, DrawFromDiscard, Discard, Knock, Pay, Bank, IsDealt, Shuffle, Show, LeaveTable, Win = Value
  
  implicit val format: Format[ThirtyOneAction] = Json.formatEnum(this)
}