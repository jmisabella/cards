package cards.models.classes.hand

import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._

// object HandPredicate extends Enumeration {
//   type HandPredicate = Value
//   val FromSplit = Value
// }

// bets: bets placed on the hand (map with player id as key and bet placed as value)
// wins: whether hand wins: when specified, true indicating hand won and false indicating a loss, bets could then be settled
case class Hand(hand: Seq[Card] = Nil, bets: Map[String, Int] = Map(), wins: Option[Boolean] = None)
