package cards.models.classes

import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._

// bets: bets placed on the hand (map with player id as key and bet placed as value)
// handWins: when specified, true indicating hand won and false indicating a loss, bets could then be settled
case class HandBet(hand: Seq[Card], bets: Map[String, Int] = Map(), handWins: Option[Boolean] = None)
