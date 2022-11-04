package cards.behaviors.controller

import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.behaviors.betting.BlackjackBetting
import cards.behaviors.play.BlackjackPlay
import cards.classes.state.{ BlackjackPlayerState, BlackjackGameState }
import cards.classes.{ Card, Deck }
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._

trait BlackjackController { 
  type BETTING <: BlackjackBetting
  type PLAY <: BlackjackPlay
  val betting: BETTING
  val play: PLAY

  def next(game: BlackjackGameState): BlackjackGameState = {
    val shuffleLimit: Int = game.players.flatMap(_.hands).length * 5
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
    if (betting.isTimeToSettle(game)) {
      // IMPORTANT: // TODO: after bets are settled, how do we get from history showing Wins, Losses, Ties back to an empty history ?????
      println("SETTLING")
      return betting.settleBets(game)
    }
    if (betting.isTimeToPlaceNewBets(game)) {
      println("BETTING")
      val adjustedStrategy: BlackjackGameState = betting.alterBettingStrategy(game.currentPlayer(), game) 
      val adjustedBetting: BlackjackGameState = betting.alterMinBet(adjustedStrategy.currentPlayer(), adjustedStrategy)
      return betting.placeBet(adjustedBetting) // TODO: test
    }
    if (play.isTimeForDealerToPlay(game)) {
      println("DEALER PLAYING")
      return play.dealerPlay(game)
    } else if (play.isTimeToDeal(game) && game.deck.cards.length >= shuffleLimit) {
      println("DEALING")
      return play.deal(game)
    } else if (play.isTimeToDeal(game) && game.deck.cards.length < shuffleLimit) {
      println("SHUFFLING")
      return game.copy(deck = Deck(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), 1), history = game.history ++ Seq(Action("Dealer", Shuffle)))
    } else if (play.isTimeToPlay(game)) {
      println("PLAYING")
      return play.playHand(game)
    } else {
      return game
    }
  }

  def init(playerNames: Seq[String], tokens: Int): BlackjackGameState = {
    val players: Seq[BlackjackPlayerState] = for (player <- playerNames) yield BlackjackPlayerState(s"$player", tokens)
    BlackjackGameState(players = players, minimumBet = (.025 * tokens).toInt)
  }

  def init(playerCount: Int = 1, tokens: Int = 2000): BlackjackGameState = {
    val players: Seq[String] = for (i <- 0 to playerCount) yield s"player${i+1}"
    init(players, tokens)
  }
}
