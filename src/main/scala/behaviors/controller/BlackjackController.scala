package cards.behaviors.controller

import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.behaviors.betting.BlackjackBetting
import cards.classes.bettingstrategy.BlackjackBettingStrategy
import cards.classes.bettingstrategy.BlackjackBettingStrategy._
import cards.behaviors.play.BlackjackPlay
import cards.classes.state.{ BlackjackPlayerState, BlackjackGameState }
import cards.classes.{ Card, Deck }
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._
import scala.annotation.tailrec

trait BlackjackController extends Controller[BlackjackPlayerState, BlackjackAction, BlackjackGameState] { 
  type BETTING <: BlackjackBetting
  type PLAY <: BlackjackPlay
  val betting: BETTING
  val play: PLAY

  private def go(game: BlackjackGameState): BlackjackGameState = {
    if (game.deck.length == 0) {
      throw new IllegalStateException(
        s"Cannot proceed to next state because deck is empty")
    }
    if (game.players.length == 0) {
      throw new IllegalStateException(
        s"Cannot proceed to next state because there are no players")
    }
    if (game.currentPlayerIndex.isEmpty && (betting.isTimeToPlaceNewBets(game) || betting.isTimeToSettle(game))) {
      return game.copy(currentPlayerIndex = Some(0), currentHandIndex = Some(0))
    }
    // if (!game.currentHandIndex.isEmpty && game.players(game.currentHandIndex.getOrElse(0)).bank < 0) { // TODO: does this prevent the index-out-of-bounds exception?
    if (!game.currentHandIndex.isEmpty && game.currentHandIndex.getOrElse(0) < game.players.length && game.players(game.currentHandIndex.getOrElse(0)).bank < 0) {
      val newHistory: Seq[Action[BlackjackAction]] = Seq(Action(game.currentPlayer().id, LeaveTable))
      val updatedPlayerIndex: Option[Int] = game.currentPlayerIndex match {
        case None => None 
        case Some(0) => Some(0)
        case Some(i) => Some(i - 1)
      }
      return game.copy(players = game.players.filter(_.id != game.currentPlayer().id), history = game.history ++ newHistory, currentPlayerIndex = updatedPlayerIndex)
    }
    val shuffleLimit: Int = (game.players.flatMap(_.hands).length + 1) * 5
    if (betting.isTimeToSettle(game)) {
      return betting.settleBets(game)
    }
    if (betting.isTimeToPlaceNewBets(game)) {
      val adjustedStrategy: BlackjackGameState = betting.alterBettingStrategy(game.currentPlayer(), game) 
      val adjustedBetting: BlackjackGameState = betting.alterMinBet(adjustedStrategy.currentPlayer(), adjustedStrategy)
      return betting.placeBet(adjustedBetting)
    }
    if (play.isTimeForDealerToPlay(game)) {
      // THIS HASN'T HAPPENED SINCE, SHOULD TEST THIS: IMPORTANT: TODO: when a player splits with more than one hand, isTimeToSettle is never true, so dealer continues playing infinitly
      return play.dealerPlay(game)
    } else if (play.isTimeToDeal(game) && game.deck.cards.length >= shuffleLimit) {
      return play.deal(game)
    } else if (play.isTimeToDeal(game) && game.deck.cards.length < shuffleLimit) {
      return game.copy(deck = Deck(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), 1), history = game.history ++ Seq(Action("Dealer", Shuffle)))
    } else if (play.isTimeToPlay(game)) {
      return play.playHand(game)
    } else {
      return game
    }
  }

  // iterations: number of iterations to take (number of moves)
  // purgeHistory: whether to purge history after all players have completed the round
  // serialize is an optional function which converts a blackjack action to text
  override def next(game: BlackjackGameState, iterations: Int = 1, purgeHistoryAfterRound: Boolean = true, serialize: Action[BlackjackAction] => String): BlackjackGameState = {
    def turns(game: BlackjackGameState, iterations: Int): BlackjackGameState = {
      def turn(game: BlackjackGameState, serialize: Action[BlackjackAction] => String): BlackjackGameState = {
        val next = go(game)
        (purgeHistoryAfterRound, purgeHistory(game, next)) match {
          case (false, _) => next
          case (_, false) => next
          case (true, true) => {
            for (a <- next.history) {
              val printed: String = serialize(a)
              if (printed != "") {
                // print history before purging it
                println(printed)
              }
            }
            // purge history
            next.copy(history = Nil)
          }
        }
      } 
      var state = game
      for (i <- (0 to iterations)) {
        try {
          state = turn(state, serialize)
        } catch {
          case _: IllegalStateException => try {
            state = turn(state, serialize)
          } catch { 
            case _: IllegalStateException => state = turn(state, serialize)
          }
        }
      }
      state
    }
    turns(game, iterations)
  }

  def init(playerNames: Seq[String], tokens: Int, strategy: BlackjackBettingStrategy, deckCount: Int): BlackjackGameState = {
    val players: Seq[BlackjackPlayerState] = for (player <- playerNames) yield BlackjackPlayerState(s"$player", tokens, bettingStrategy = strategy)
    BlackjackGameState(players = players, minimumBet = (.025 * tokens).toInt)
  }

  def init(playerCount: Int = 1, tokens: Int = 2000, strategy: BlackjackBettingStrategy = NegativeProgression, deckCount: Int = 1): BlackjackGameState = {
    val players: Seq[String] = for (i <- 0 until playerCount) yield s"player${i+1}"
    init(players, tokens, strategy, deckCount)
  }
}
