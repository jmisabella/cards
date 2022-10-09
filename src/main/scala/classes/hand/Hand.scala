package cards.classes.hand

import cards.classes.{ Card, Rank, Suit, Deck }
import cards.classes.Rank._
import cards.classes.Suit._

// bets: bets placed on the hand (map with player id as key and bet placed as value)
// wins: whether hand wins: when specified, true indicating hand won and false indicating a loss, bets could then be settled
case class Hand(hand: Seq[Card] = Nil, bets: Map[String, Int] = Map(), wins: Option[Boolean] = None)
object Hand {
  val empty: Hand = Hand(Nil) // helps with readability (e.g. dealerHand = Hand.empty)
}