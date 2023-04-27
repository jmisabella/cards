package cards.classes.state

import cards.classes.state.{ PlayerState, GameState, PlayerSummary, PlayerSummaries }
import cards.classes.{ Card, Rank, Suit, Deck }
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.hand.Hand
import cards.classes.options.blackjack.BlackjackOptions
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._
import cards.classes.bettingstrategy.BlackjackBettingStrategy._
import play.api.libs.json.{ Json, Format }

// id: player's unique identifier
// bank: player's available tokens
// handsAndBets: player has 1 or more Hands, with each Hand containing its cards as well as players' bets placed on the hand
// minBetMultiplier: (only applicable to non-players) when betting normally, how many times the minimum bet should the player bet
// maxBet: (only applicable to non-players) when specified, maximum bet a player wishes to make for any hand
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
// bankedLastBettingAmountUpdate: to be reset every 25 games to update to bank amount, used to determine whether to increase or decrease  
//                            player's minimum bet, based on whether bank increases or decreases after 25 games
// bankedLastStrategyUpdate: to be reset every 250 games to update to current bank amount, used to determine whether to change betting strategy, 
//                             based on whether bank increases by 15% after 250 games
// completedHands: starts at 0 and increases with every hand won or lost, the number of completed hands is used to track updating every 25 and 250 
//                 hands to determine when to refresh bankEvery25Hands and bankEvery250Hands
// highestBank: tracks the highest bank amount ever achieved by the player
// rounds: tracks the number of rounds played by the player
// goal: overall goal upon which reaching player would leave the table
case class BlackjackPlayerState(
  override val id: String, 
  override val bank: Int = 200, 
  handsAndBets: Seq[Hand] = Nil, 
  minBetMultiplier: Double = 1.0,
  maxBet: Option[Int] = None,
  bettingStrategy: BlackjackBettingStrategy = NegativeProgression,
  oscarsGoalMultiplier: Double = 1.25, 
  oscarsGoal: Int = 0,
  bankedLastBettingAmountUpdate: Int = 1,
  bankedLastStrategyUpdate: Int = 1,
  completedHands: Int = 0,
  override val highestBank: Int = 0,
  override val rounds: Int = 0,
  goal: Int = 30000) extends PlayerState {
  
    val hands: Seq[Seq[Card]] = handsAndBets.map(_.hand)
    val oscarsGoalMet: Boolean = bank >= oscarsGoal 
    require(minBetMultiplier >= 1.0)
    require(oscarsGoalMultiplier >= 1.0)

    def updateHand(beforeCards: Seq[Card], updatedCards: Seq[Card]): BlackjackPlayerState = updatedCards match {
      case Nil => this // in blackjack, cards are only ever added but are never discarded, so don't allow update to empty hand
      case _ => this.copy(handsAndBets = handsAndBets.map { h => 
        if (h.hand.sorted == beforeCards.sorted) 
          h.copy(hand = updatedCards) 
        else 
          h
      })
    }

    def clearHands(): BlackjackPlayerState = this.copy(handsAndBets = Nil)
}

object BlackjackPlayerState {
  def apply(hands: Seq[Seq[Card]], id: String, bank: Int): BlackjackPlayerState = BlackjackPlayerState(id, bank, handsAndBets = hands.map(h => Hand(h)))
  def apply(id: String, hand: Seq[Card], bank: Int): BlackjackPlayerState = BlackjackPlayerState(Seq(hand), id, bank) 
  def apply(summary: PlayerSummary): BlackjackPlayerState = BlackjackPlayerState(summary.id, Nil, summary.bank)
}

// dealer's hand's head is the face-up card, all other cards are face down
// completedPlayers: players who have left the table due to either insufficient funds or reaching the goal
case class BlackjackGameState(
  override val players: Seq[BlackjackPlayerState] = Nil,
  override val currentPlayerIndex: Option[Int] = None,
  override val history: Seq[Action[BlackjackAction]] = Nil,
  override val deck: Deck = Deck(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), 1),
  currentHandIndex: Option[Int] = None, 
  options: BlackjackOptions = BlackjackOptions(),
  dealerHand: Hand = Hand.empty, // bets are only placed on dealer's hand when purchasing insurance
  minimumBet: Int = 20,
  maximumBet: Int = 999999,
  round: Int = 1,
  completedPlayers: Seq[BlackjackPlayerState] = Nil) extends GameState[BlackjackPlayerState, BlackjackAction] {

    def currentCards(): Seq[Card] = current(currentPlayer().hands, currentHandIndex)
    def nextHandIndex(): Int = nextIndex(currentPlayer().hands, currentHandIndex)
    // TODO: this is not used anywhere other than in a unit test, should this be removed? 
    def isLastHand(): Boolean = 
      currentPlayerIndex.isDefined && currentPlayer().hands.length > 0 && currentHandIndex == Some(currentPlayer().hands.length - 1)
    
    def currentHand(): Hand = players.flatMap(_.handsAndBets).filter(_.hand == currentCards()).head

    // to next hand (or player) based on whether current action is Stand or Surrender, or if current hand busted
    def toNextHand(action: BlackjackAction, busted: Boolean): BlackjackGameState = {
      if (!busted || !Seq(Stand, Surrender).contains(action)) {
        // no bust and last action was not Stand or Surrender, so no change
        this
      }
      // else bust occurred or last action was Stand or Surrender, so iterate to the next hand
      (currentPlayerIndex, currentHandIndex, isLastHand()) match {
        case (None, _, _) => this // no current player, nothing to iterate
        // current hand index is undefined, and we're not yet at the last player, so iterate to next player
        case (Some(n), None, _) if (n < players.length - 1) => this.copy(currentPlayerIndex = Some(n + 1))
        // current hand index is undefined and we're at last player, so current player index is now undefined 
        case (Some(_), None, _) => this.copy(currentPlayerIndex = None)
        // player's last hand, and this is not the last player so iterate to next player and set current hand index to 0
        case (Some(n), Some(h), true) if (n < players.length - 1) => this.copy(currentPlayerIndex = Some(n + 1), currentHandIndex = Some(0))
        // player's last hand, and this the last player, so current player index is now undefined 
        case (Some(_), Some(h), true) => this.copy(currentPlayerIndex = None, currentHandIndex = Some(0))
        // player's not yet at last hand, so iterate to the next hand
        case (Some(_), Some(h), false) => this.copy(currentHandIndex = Some(h + 1))
      }
    }

    def playerHistory(playerId: String): Seq[Action[BlackjackAction]] = history.filter(a => a.playerId == playerId)

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

// game summary
case class CompletedBlackjack(players: PlayerSummaries, history: Seq[Action[BlackjackAction]]) {
  override def toString(): String = s"""{"blackjack": {"players": ${players.toString()}, "history": ${history.mkString("[", ", ", "]")}}}""" 
}
object CompletedBlackjack {
  def apply(game: BlackjackGameState): CompletedBlackjack = {
    CompletedBlackjack(PlayerSummaries((game.completedPlayers ++ game.players).map(PlayerSummary(_)).distinct), game.history)
  }
  implicit val format: Format[CompletedBlackjack] = Json.format[CompletedBlackjack]
}