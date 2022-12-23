package cards.classes.hand

import cards.classes.{ Card, Rank, Suit, Deck }
import cards.classes.Outcome._

// bets (if applicable to the game): bets placed on the hand (map with player id as key and bet placed as value)
// owners (if applicable to the game): i.e. poker hands often consist of face-down cards (seen by player) and face-up cards (shared by multiple players)
// outcome: whether hand wins: when specified, true indicating hand won and false indicating a loss, bets could then be settled
case class Hand(hand: Seq[Card] = Nil, bets: Map[String, Int] = Map(), owners: Seq[String] = Nil, outcome: Option[Outcome] = None)
object Hand {
  val empty: Hand = Hand(Nil) // helps with readability (e.g. dealerHand = Hand.empty)
}