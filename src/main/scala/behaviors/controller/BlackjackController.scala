package cards.behaviors.controller

import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.behaviors.betting.BlackjackBetting
import cards.classes.bettingstrategy.BlackjackBettingStrategy
import cards.classes.bettingstrategy.BlackjackBettingStrategy._
import cards.behaviors.play.BlackjackPlay
import cards.classes.state.{ BlackjackPlayerState, BlackjackGameState, CompletedBlackjack }
import cards.classes.{ Card, Deck }
import cards.classes.hand.Hand
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.options.blackjack.BlackjackOptions
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
      return game.copy(
        completedPlayers = game.completedPlayers ++ Seq(game.currentPlayer()), 
        players = game.players.filter(_.id != game.currentPlayer().id), 
        history = game.history ++ newHistory, 
        currentPlayerIndex = updatedPlayerIndex)
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

  def init(playerNames: Seq[String], options: BlackjackOptions): BlackjackGameState = {
    def removeFirst[T](list: List[T])(pred: (T) => Boolean): List[T] = {
      val (before, atAndAfter) = list span (x => !pred(x))
      before ::: atAndAfter.drop(1)
    }
    val goal = options.initialBank * 10
    var players: Seq[BlackjackPlayerState] = for (player <- playerNames) yield BlackjackPlayerState(s"$player", options.initialBank, goal = goal, bettingStrategy = NegativeProgression)
    var dealerHand: Hand = Hand()
    var history: Seq[Action[BlackjackAction]] = Nil
    val minimum: Int = (.025 * options.initialBank).toInt match {
      case 0 => 1
      case n => n 
    } 
    val deck = Deck(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), options.deckCount)
    var remaining: List[Card] = deck.cards
    if (options.playerInitialRanks.length == 2 && options.dealerInitialRanks.length == 2) {
      val playerRankFirstCards: Seq[Card] = deck.cards.filter(_.rank == options.playerInitialRanks.head).take(players.length)
      val playerRankSecondCards: Seq[Card] = deck.cards.filter(_.rank == options.playerInitialRanks.tail.head).reverse.take(players.length)
      val dealtPlayerHands: Seq[Seq[Card]] = for ((c1, c2) <- (playerRankFirstCards zip playerRankSecondCards)) yield {
        Seq(c1, c2)
      }
      val dealerRankFirstCard: Card = deck.cards.filter(_.rank == options.dealerInitialRanks.head).take(1).head
      val dealerRankSecondCard: Card = deck.cards.filter(_.rank == options.dealerInitialRanks.tail.head).take(1).head
      for (card <- (playerRankFirstCards ++ playerRankSecondCards ++ Seq(dealerRankFirstCard, dealerRankSecondCard))) {
        remaining = removeFirst(remaining)(c => c.rank == card.rank)
      }
      players = for ( (p, cs) <- (players zip dealtPlayerHands)) yield {
        p.copy(handsAndBets = Seq(Hand(cs, Map(p.id -> minimum))))
      }
      dealerHand = Hand(Seq(dealerRankFirstCard, dealerRankSecondCard), Map())
      for (p <- players) {
        // for Bet action, specify betting strategy and min bet multiplier in action 
        history = history ++ Seq(Action(p.id, Bet, actionTokens = Some(minimum), bettingStrategy = Some(p.bettingStrategy.toString()), minBetMultiplier = Some(1.0)))
      }
      for (p <- players) {
        history = history ++ Seq(Action(p.id, IsDealt, actionCards = p.hands.headOption.getOrElse(Nil), afterCards = p.hands))
      }
      history = history ++ Seq(Action("dealer", IsDealt, actionCards = dealerHand.hand, afterCards = Seq(dealerHand.hand)))
    }
    if (options.initialBettingStrategy.isDefined) {
      players = players.map(p => p.copy(bettingStrategy = BlackjackBettingStrategy.withNameOpt(options.initialBettingStrategy.get).getOrElse(NegativeProgression)))
      history = history.map(a => if (a.action == Bet) a.copy(bettingStrategy = options.initialBettingStrategy) else a)
    }
    if (options.alternateBettingStrategy.isDefined) {
      players = players.map(p => p.copy(alternateBettingStrategy = options.alternateBettingStrategy.getOrElse(true)))
    }
    BlackjackGameState(
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0), 
      players = players, 
      minimumBet = minimum, 
      options = options,
      deck = deck.copy(cards = remaining),
      dealerHand = dealerHand,
      history = history)
  } 
  def init(playerCount: Int, options: BlackjackOptions): BlackjackGameState = {
    val players: Seq[String] = for (i <- 0 until playerCount) yield s"player${i+1}"
    init(players, options)
  }
  
  def init(playerCount: Int, initialBank: Int): BlackjackGameState = {
    val players: Seq[String] = for (i <- 0 until playerCount) yield s"player${i+1}"
    init(players, BlackjackOptions(initialBank = initialBank))
  }

  def init(state: CompletedBlackjack): BlackjackGameState = {
    val remaining: Seq[BlackjackPlayerState] = state.players.players.filter(p => p.bank > 0).map(p => BlackjackPlayerState(p.id, p.bank, goal = p.highestBank * 4))
    remaining match {
      case Nil => init(0, 0) // no players left
      case xs => {
        val lowest: Int = xs.map(_.bank).min
        val minimum: Int = (.025 * lowest).toInt match {
          case 0 => 1
          case n => n 
        } 
        BlackjackGameState(players = xs.filter(p => p.bank >= minimum), minimumBet = minimum)
      } 
    }
  }

  def continuedState(previousGame: CompletedBlackjack, newGoal: Int = 30000, strategy: BlackjackBettingStrategy = NegativeProgression, deckCount: Int = 1): BlackjackGameState = {
    val remaining = previousGame.players.players.filter(p => p.bank > 0)
    remaining match {
      case Nil => init(0, 0) // no players left
      case xs => {
        val players: Seq[BlackjackPlayerState] = remaining.map(BlackjackPlayerState(_).copy(bettingStrategy = strategy, goal = newGoal))
        val lowest: Int = players.map(_.bank).min
        val minimum: Int = (.025 * lowest).toInt match {
          case 0 => 1
          case n => n 
        } 
        BlackjackGameState(players = players.filter(p => p.bank >= minimum), minimumBet = minimum)
      } 
    }
  }

  def completeGame(state: CompletedBlackjack, deckCount: Int): CompletedBlackjack = {
    val highest: Int = state.players.players.map(_.bank).max
    val initialState: BlackjackGameState = continuedState(state, newGoal = highest * 10, deckCount = deckCount)
    completeGame(initialState)
  }
  
  def completeGame(state: BlackjackGameState): CompletedBlackjack = {
    val finalState: BlackjackGameState = play(state.copy(players = state.players.map(p => p.copy(goal = p.goal * 2))), 40000 * state.players.length)
    CompletedBlackjack(finalState)
  }
}
