package cards.classes.actions

import cards.classes.Card
import cards.classes.actions.ThirtyOneAction._
import cards.classes.actions.BlackjackAction._
// import play.api.libs.json. { Json, Format }
import cards.utilities.GsonListAdapter
import com.google.gson.Gson

// afterCards is seq of seq of cards, since cards could be split
case class Action[A <: Enumeration#Value](
  playerId: String, 
  action: A, 
  actionCards: Seq[Card] = Nil, 
  actionTokens: Int = 0, 
  beforeCards: Seq[Card] = Nil, 
  afterCards: Seq[Seq[Card]] = Nil) {
  
    override def toString(): String = 
     new Gson().toJson(this)
    //  GsonListAdapter.toJson[Action[A]](this) 
      // (Json.obj(
      //   "playerId" -> playerId,
      //   "action" -> action.toString(),
      //   "actionCards" -> actionCards,
      //   "actionTokens" -> actionTokens,
      //   "beforeCards" -> beforeCards,
      //   "afterCards" -> afterCards
      // )).toString()
}

// object Action {
//   implicit val format1: Format[Action[ThirtyOneAction]] = Json.format[Action[ThirtyOneAction]]
//   implicit val format2: Format[Action[BlackjackAction]] = Json.format[Action[BlackjackAction]]
// }

case class Actions[A <: Enumeration#Value](actions: Seq[Action[A]]) {
  override def toString(): String = (actions.map(_.toString())).mkString("""{"actions":[""", "," ,"]}") 
}

// object Actions {
//   implicit val format1: Format[Actions[ThirtyOneAction]] = Json.format[Actions[ThirtyOneAction]]
//   implicit val format2: Format[Actions[BlackjackAction]] = Json.format[Actions[BlackjackAction]]
// }
