package cards.models.classes.state

import cards.models.classes.state.{ PlayerState, GameState }
// import cards.models.classes.{ Card, Deck }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.options.BlackjackOptions
// import cards.models.classes.options.BlackjackPayout._
// import cards.models.classes.options.Surrender._
// import cards.models.classes.options.DealerHitLimit._
// import cards.models.classes.options.ResplitLimit._
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._

case class BlackjackPlayerState(id: String, bank: Int = 0, handsAndBets: Seq[Hand] = Nil) extends PlayerState {
  val hands: Seq[Seq[Card]] = handsAndBets.map(_.hand)
  // def playerBet(playerId: String): Option[(Seq[Card], Int)] = { 
  //   (for {
  //     x <- handsAndBets
  //     if (x.bets.keys.toSeq.contains(playerId))
  //   } yield (x.hand, x.bets.filter(_._1 == playerId).values.head)
  //   ).headOption
  // }
}

object BlackjackPlayerState {
  def apply(hands: Seq[Seq[Card]], id: String, bank: Int): BlackjackPlayerState = BlackjackPlayerState(id, bank, hands.map(h => Hand(h)))
  def apply(id: String, hand: Seq[Card], bank: Int): BlackjackPlayerState = BlackjackPlayerState(Seq(hand), id, bank) 
}

// dealer's hand's head is the face-up card, all other cards are face down
case class BlackjackGameState(
  override val players: Seq[BlackjackPlayerState] = Nil,
  override val currentPlayerIndex: Option[Int] = None,
  override val history: Seq[Action[BlackjackAction]] = Nil,
  override val deck: Deck = Deck(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), 1),
  currentHandIndex: Option[Int] = None, 
  options: BlackjackOptions = BlackjackOptions(),
  dealerHand: Hand = Hand(), // bets are only placed on dealer's hand when purchasing insurance
  insurance: Map[String, Int] = Map(), 
  minimumBet: Int = 1,
  maximumBet: Int = 999999) extends GameState[BlackjackPlayerState, BlackjackAction] {

    def currentHand(): Seq[Card] = current(currentPlayer().hands, currentHandIndex)
    def nextHandIndex(): Int = nextIndex(currentPlayer().hands, currentHandIndex)
    // TODO: test 
    def isLastHand(): Boolean = currentHandIndex == Some(currentPlayer().hands.length)

}
