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
// predicates: can be used to track whether certain preconditions have occurred:
//    e.g. whether hand came from a split, whether it came from split aces, etc...
case class Hand(hand: Seq[Card], bets: Map[String, Int] = Map(), wins: Option[Boolean] = None, predicates: Map[String, Boolean] = Map())
