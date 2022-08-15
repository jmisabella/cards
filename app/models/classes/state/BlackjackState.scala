package cards.models.classes.state

import cards.models.classes.Card
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.options.Surrender._
import cards.models.classes.options.DealerHitLimit._
import cards.models.classes.options.ResplitLimit._
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._

case class BlackjackPlayerState(id: String, bank: Int, hands: Seq[Seq[Card]] = Nil)

// dealer's hand's head is the face-up card, all other cards are face down
case class BlackjackGameState(
  options: BlackjackOptions, 
  dealerHand: Seq[Card] = Nil, 
  players: Seq[BlackjackPlayerState] = Nil, 
  pot: Int = 0, 
  currentPlayerIndex: Option[Int] = None,
  winningPlayerId: Option[String] = None,
  history: Seq[Action[BlackjackAction]])



