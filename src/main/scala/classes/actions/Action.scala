package cards.classes.actions

import cards.classes.Card
import cards.classes.actions.ThirtyOneAction._
import cards.classes.actions.BlackjackAction._
import play.api.libs.json. { Json, Format }

// afterCards is seq of seq of cards, since cards could be split
case class Action[A <: Enumeration#Value](
  playerId: String, 
  action: A, 
  actionCards: Seq[Card] = Nil, 
  actionTokens: Option[Int] = None, 
  beforeCards: Seq[Card] = Nil, 
  afterCards: Seq[Seq[Card]] = Nil,
  beforeTokens: Option[Int] = None,
  afterTokens: Option[Int] = None,
  bettingStrategy: Option[String] = None,
  minBetMultiplier: Option[Double] = None//,
  // additionalData: Map[String, String] = Map() // TODO: put bet multiplier and betting strategy in additional data map
  ) {
    override def toString(): String = 
      (Json.obj(
        "playerId" -> playerId,
        "action" -> action.toString(),
        "actionCards" -> actionCards,
        "actionTokens" -> actionTokens,
        "beforeCards" -> beforeCards,
        "afterCards" -> afterCards,
        "beforeTokens" -> beforeTokens,
        "afterTokens" -> afterTokens,
        "bettingStrategy" -> bettingStrategy,
        "minBetMultiplier" -> minBetMultiplier.map(_.toString())
      )).toString()
}

object Action {
  implicit val format1: Format[Action[ThirtyOneAction]] = Json.format[Action[ThirtyOneAction]]
  implicit val format2: Format[Action[BlackjackAction]] = Json.format[Action[BlackjackAction]]
}

case class Actions[A <: Enumeration#Value](actions: Seq[Action[A]]) {
  override def toString(): String = (actions.map(_.toString())).mkString("""{"actions":[""", "," ,"]}") 
}

object Actions {
  implicit val format1: Format[Actions[ThirtyOneAction]] = Json.format[Actions[ThirtyOneAction]]
  implicit val format2: Format[Actions[BlackjackAction]] = Json.format[Actions[BlackjackAction]]
}
