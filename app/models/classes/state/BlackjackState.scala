package cards.models.classes.state

import cards.models.classes.state.{ PlayerState, GameState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.options.Surrender._
import cards.models.classes.options.DealerHitLimit._
import cards.models.classes.options.ResplitLimit._
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._

// handsAndBets: sequence of tuples, where each tuple's _1 is the actual hand, and the tuple's _2 is bets placed on the hand (map with player id as key and bet placed as value)
case class BlackjackPlayerState(id: String, bank: Int = 0, handsAndBets: Seq[(Seq[Card], Map[String, Int])] = Nil) extends PlayerState {
  val hands: Seq[Seq[Card]] = handsAndBets.map(_._1)
  def playerBet(playerId: String): Option[(Seq[Card], Int)] = { 
    (for {
      x <- handsAndBets
      if (x._2.keys.toSeq.contains(playerId))
    } yield (x._1, x._2.filter(_._1 == playerId).values.head)
    ).headOption
  }
}

object BlackjackPlayerState {
  def apply(hands: Seq[Seq[Card]], id: String, bank: Int): BlackjackPlayerState = BlackjackPlayerState(id, bank, hands.map(h => (h, Map(): Map[String, Int]))) 
  def apply(id: String, hand: Seq[Card], bank: Int): BlackjackPlayerState = BlackjackPlayerState(Seq(hand), id, bank) 
}

// dealer's hand's head is the face-up card, all other cards are face down
case class BlackjackGameState(
  override val players: Seq[BlackjackPlayerState] = Nil,
  override val currentPlayerIndex: Option[Int] = None,
  override val history: Seq[Action[BlackjackAction]] = Nil,
  override val deck: Deck = Deck(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), 1),
  currentHand: Option[Seq[Card]] = None,
  options: BlackjackOptions = BlackjackOptions(),
  dealerHand: Seq[Card] = Nil) extends GameState[BlackjackPlayerState, BlackjackAction] {

    def playerBets(playerId: String): Seq[(Seq[Card], Int)] = {
      for {
        player <- players;
        bet <- player.playerBet(playerId)
      } yield bet
    }
}
