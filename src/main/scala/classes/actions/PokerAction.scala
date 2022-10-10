package cards.classes.actions

// import play.api.libs.json.{ Json, Format }

object PokerAction extends Enumeration {
  type PokerAction = Value
  val Draw, Discard, Fold = Value
  
  // implicit val format: Format[PokerAction] = Json.formatEnum(this)
}
