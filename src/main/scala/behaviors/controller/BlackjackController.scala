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

trait BlackjackController { 
  type BETTING <: BlackjackBetting
  type PLAY <: BlackjackPlay
  val betting: BETTING
  val play: PLAY

  private def purgeHistory(previous: BlackjackGameState, current: BlackjackGameState): Boolean = previous.round != current.round

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
    if (!game.currentHandIndex.isEmpty && game.players(game.currentHandIndex.getOrElse(0)).bank < 0) {
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
      // IMPORTANT: // TODO: after bets are settled, how do we get from history showing Wins, Losses, Ties back to an empty history ?????
      // println("SETTLING")
      return betting.settleBets(game)
    }
    if (betting.isTimeToPlaceNewBets(game)) {
      // println("BETTING")
      val adjustedStrategy: BlackjackGameState = betting.alterBettingStrategy(game.currentPlayer(), game) 
      val adjustedBetting: BlackjackGameState = betting.alterMinBet(adjustedStrategy.currentPlayer(), adjustedStrategy)
      return betting.placeBet(adjustedBetting) // TODO: test
    }
    if (play.isTimeForDealerToPlay(game)) {
      // IMPORTANT: // TODO: when a player splits with more than one hand, isTimeToSettle is never true, so dealer continues playing infinitly
      // println("DEALER PLAYING")
      return play.dealerPlay(game)
    } else if (play.isTimeToDeal(game) && game.deck.cards.length >= shuffleLimit) {
      // println("DEALING")
      return play.deal(game)
    } else if (play.isTimeToDeal(game) && game.deck.cards.length < shuffleLimit) {
      // println(s"SHUFFLING: length [${game.deck.cards.length}], shuffle limit [$shuffleLimit]")
      return game.copy(deck = Deck(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), 1), history = game.history ++ Seq(Action("Dealer", Shuffle)))
    } else if (play.isTimeToPlay(game)) {
      // println("PLAYING")
      return play.playHand(game)
    } else {
      return game
    }
  }

  def next(game: BlackjackGameState, iterations: Int): BlackjackGameState = {
    val f = (a: Action[BlackjackAction]) => ""
    next(game, iterations, f) 
  }
  
  def next(game: BlackjackGameState): BlackjackGameState = {
    val f = (a: Action[BlackjackAction]) => ""
    next(game, 1, f) 
  }
  
  // serialize is an optional function which converts a blackjack action to text
  def next(game: BlackjackGameState, iterations: Int = 1, serialize: Action[BlackjackAction] => String): BlackjackGameState = {
    def turns(game: BlackjackGameState, iterations: Int): BlackjackGameState = {
      def turn(game: BlackjackGameState, serialize: Action[BlackjackAction] => String): BlackjackGameState = {
        val next = go(game)
        purgeHistory(game, next) match {
          case false => next
          case true => {
            for (a <- next.history) {
              val printed: String = serialize(a)
              if (printed != "") {
                println(printed)
              }
            }
            next.copy(history = Nil)
          }
        }
      } 
      var state = game 
      try {
        for (i <- (0 to iterations)) {
          state = turn(state, serialize)
        }
        state
      } catch {
        case _: IllegalStateException => {
          for (i <- (0 to iterations)) {
            state = turn(state, serialize)
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
    val players: Seq[String] = for (i <- 0 to playerCount) yield s"player${i+1}"
    init(players, tokens, strategy, deckCount)
  }
}
