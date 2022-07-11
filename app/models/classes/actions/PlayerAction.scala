package cards.models.classes.actions

import cards.models.classes.Card
import cards.models.classes.actions.ThirtyOneAction._
import play.api.libs.json. { Json, Format }

// TODO: behaviors, json serialization, testing
case class Action[A <: Enumeration#Value](previousCards: Seq[Card], action: A, actionCards: Seq[Card], updatedCards: Seq[Card]) {
  override def toString(): String = 
    (Json.obj(
      "previousCards" -> previousCards.mkString("[", ", ", "]"),
      "action" -> action.toString(),
      "actionCards" -> actionCards.mkString("[", ", ", "]"),
      "updatedCards" -> updatedCards.mkString("[", ", ", "]") 
    )).toString()
}

object Action {
  implicit val format1: Format[Action[ThirtyOneAction]] = Json.format[Action[ThirtyOneAction]]
}

case class Actions[A <: Enumeration#Value](actions: Seq[Action[A]]) {
  override def toString(): String = (actions.map(_.toString())).mkString("""{"actions":[""", "," ,"]}") 
}

object Actions {
  implicit val format1: Format[Actions[ThirtyOneAction]] = Json.format[Actions[ThirtyOneAction]]
}
