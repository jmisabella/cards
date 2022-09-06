package cards.models.classes.state

import cards.models.classes.state.{ PlayerState, GameState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import cards.models.classes.bettingstrategy.BlackjackBettingStrategy._

// id: player's unique identifier
// bank: player's available tokens
// handsAndBets: player has 1 or more Hands, with each Hand containing its cards as well as players' bets placed on the hand
// minBetMultiplier: (only applicable to non-players) when betting normally, how many times the minimum bet should the player bet
// maxBetMultiplier: (only applicable to non-players) how many times minimum bet without exceeding max bet should player reach 
//                                                    as a personal maximum bet
// bettingStrategy: one of Steady, Martingale, Oscars, PositiveProgression, NegativeProgression
//  * Steady - always bet same amount, regardless of wins or losses
//  * Martingale - always bet set amount after a win; this amount is multiplied x2 after 1 loss, x4 after 2 losses, x8 after 3 losses, etc...
//  * Oscars - always bet set amount after a loss; after every win, allow the won amount to ride, to double it after 2 wins; 
//             end when a specific goal is met
//  * PositiveProgression - always bet same amount after a loss; increase this amount after wins, but to in no specific intervals
//  * NegativeProgression - always bet same amound after a win; increase amount after losses, but unlike Martingale does not have 
//                          to increase by a doubled amount after each loss
// oscarsGoalMultiplier: only applicable for Oscar's betting strategy, player's bank multiplied by this value is the incremental 
//                       goal of Oscar's betting
// oscarsGoal: only applicable for Oscar's betting strategy, intermediate goal to be achieved
// bankStartEvery25Intervals: to be reset every 25 games to update to bank amount, used to determine whether to increase or decrease  
//                            player's minimum bet, based on whether bank increases or decreases after 25 games
// bankStartEvery250Intervals: to be reset every 250 games to update to current bank amount, used to determine whether to change betting strategy, 
//                             based on whether bank increases by 15% after 250 games 
case class BlackjackPlayerState(
  id: String, 
  bank: Int = 0, 
  handsAndBets: Seq[Hand] = Nil, 
  minBetMultiplier: Double = 1.0, 
  maxBetMultiplier: Double = 2.0,
  bettingStrategy: BlackjackBettingStrategy = Steady,
  oscarsGoalMultiplier: Double = 1.25, 
  oscarsGoal: Int = 0,
  bankStartEvery25Intervals: Int = 0,
  bankStartEvery250Intervals: Int = 0,
  gameCounter: Int = 0) extends PlayerState {
  
    val hands: Seq[Seq[Card]] = handsAndBets.map(_.hand)
    val oscarsGoalMet: Boolean = bank >= oscarsGoal 
    require(minBetMultiplier <= maxBetMultiplier)
    require(minBetMultiplier >= 1.0)
    require(maxBetMultiplier >= 1.0)
    require(oscarsGoalMultiplier >= 1.0)
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
  dealerHand: Hand = Hand.empty, // bets are only placed on dealer's hand when purchasing insurance
  insurance: Map[String, Int] = Map(), 
  minimumBet: Int = 1,
  maximumBet: Int = 999999) extends GameState[BlackjackPlayerState, BlackjackAction] {

    def currentHand(): Seq[Card] = current(currentPlayer().hands, currentHandIndex)
    def nextHandIndex(): Int = nextIndex(currentPlayer().hands, currentHandIndex)
    def isLastHand(): Boolean = currentHandIndex == Some(currentPlayer().hands.length - 1)

    // true indicates a win, false indicates a loss; order is in the original order played, so most recent is last item
    def winningHistory(playerId: String): Seq[Boolean] = {
      history
        .filter ( a => a.playerId == playerId && Seq(Win, Lose).contains(a.action))
        .map ( a => a.action match {
          case Win => true
          case Lose => false
        })
    }
}
