package cards.classes.options

import play.api.libs.json.{ Json, Format }

// fixed-limit: bets can only be raised in increments of the ante amount
// pot-limit: player may bet or raise by any amount up to the pot (meaning the cumulated pot as well as all previous bets of this round)
// no-limit: player is allowed to wager their entire betting stack at any point they are allowed to bet
object BettingStructure extends Enumeration {
  type BettingStructure = Value
  val FixedLimit, PotLimit, NoLimit= Value
  implicit val format: Format[BettingStructure] = Json.formatEnum(this)
}

