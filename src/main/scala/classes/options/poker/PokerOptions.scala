package cards.classes.options.poker

import play.api.libs.json.{ Json, Format }

// fixed-limit: bets can only be raised in increments of the ante amount
// pot-limit: player may bet or raise by any amount up to the pot (meaning the cumulated pot as well as all previous bets of this round)
// no-limit: player is allowed to wager their entire betting stack at any point they are allowed to bet
object BettingStructure extends Enumeration {
  type BettingStructure = Value
  val FixedLimit, PotLimit, NoLimit = Value
  implicit val format: Format[BettingStructure] = Json.formatEnum(this)
}

// Draw poker: Games in which players are dealt a complete hand, hidden, and then improve it by replacing cards. Five-card draw is the most common.
// Stud poker: Games in which each player receives a combination of face-up cards and face-down cards in multiple betting rounds. 5-card & 7-card stud are the most common.
// Community poker: Games in which each player's incomplete hidden hand is combined with shared face-up cards. Texas and Omaha Hold-Em are the most common.
object PokerVariant extends Enumeration {
  type PokerVariant = Value
  val FiveCardDraw, FiveCardStud, SevenCardStud, TexasHoldEm, OmahaHoldEm = Value
  implicit val format: Format[PokerVariant] = Json.formatEnum(this)
}

// TODO: logic for determining number of decks to use (based on number of players and also sometimes takes into account the game variation as well)

// TODO: applicable only to some games such as Omaha, option showing how many community cards and how many personal cards must be selected when forming hand
