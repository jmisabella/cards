package cards.models.classes.actions

import play.api.libs.json.{ Json, Format }

object BlackJackAction extends Enumeration {
  type BlackJackAction = Value
  val Hit, Stand, Insurance, Split, DoubleDown = Value
  
  implicit val format: Format[BlackJackAction] = Json.formatEnum(this)
}

