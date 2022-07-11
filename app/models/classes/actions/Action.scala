package cards.models.classes.actions

import cards.models.classes.Card
import cards.models.classes.actions.ThirtyOneAction._
import cards.models.classes.actions.BlackJackAction._
import play.api.libs.json. { Json, Format }

// TODO: behaviors, json serialization, testing
case class Action[A <: Enumeration#Value](before: Seq[Card], after: Seq[Card], action: A, actionCards: Seq[Card] = Nil, actionTokens: Int = 0) {
  override def toString(): String = 
    (Json.obj(
      "before" -> before.mkString("[", ", ", "]"),
      "after" -> after.mkString("[", ", ", "]"),
      "action" -> action.toString(),
      "actionCards" -> actionCards.mkString("[", ", ", "]"),
      "actionTokens" -> actionTokens.toString()
    )).toString()
}

object Action {
  implicit val format1: Format[Action[ThirtyOneAction]] = Json.format[Action[ThirtyOneAction]]
  implicit val format2: Format[Action[BlackJackAction]] = Json.format[Action[BlackJackAction]]
}

case class Actions[A <: Enumeration#Value](actions: Seq[Action[A]]) {
  override def toString(): String = (actions.map(_.toString())).mkString("""{"actions":[""", "," ,"]}") 
}

object Actions {
  implicit val format1: Format[Actions[ThirtyOneAction]] = Json.format[Actions[ThirtyOneAction]]
  implicit val format2: Format[Actions[BlackJackAction]] = Json.format[Actions[BlackJackAction]]
}
