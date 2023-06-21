package cards.classes.actions

import play.api.libs.json.{ Json, Format }

object BlackjackAction extends Enumeration {
  type BlackjackAction = Value
  val Hit, 
    Stand, 
    BuyInsurance, 
    Split, 
    DoubleDown, 
    Surrender, 
    IsDealt, 
    Bet, 
    Win, 
    Lose, 
    Tie, 
    Shuffle, 
    ShowCards, 
    InsufficientFunds, 
    GoalReached, 
    LeaveTable,
    Bust,
    Blackjack = Value
  
  implicit val format: Format[BlackjackAction] = Json.formatEnum(this)
}
