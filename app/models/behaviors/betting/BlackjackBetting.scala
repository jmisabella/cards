package cards.models.behaviors.betting

import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.hand.Hand
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.options.Surrender._
import cards.models.classes.options.DealerHitLimit._
import cards.models.classes.options.ResplitLimit._
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import cards.models.classes.state.{ BlackjackPlayerState, BlackjackGameState }
import cards.models.classes.bettingstrategy.BlackjackBettingStrategy._
import cards.models.behaviors.evaluation.BlackjackHandEvaluation

trait BlackjackBetting {
  type EVAL <: BlackjackHandEvaluation 
  val evaluation: EVAL

  def getPlayerBet(playerState: BlackjackPlayerState, playerId: String): Option[(Seq[Card], Int)] = { 
    (for {
      x <- playerState.handsAndBets
      if (x.bets.keys.toSeq.contains(playerId))
    } yield (x.hand, x.bets.filter(_._1 == playerId).values.head)
    ).headOption
  }

  def getPlayerBets(game: BlackjackGameState, playerId: String): Seq[(Seq[Card], Int)] = {
    for {
      player <- game.players
      bet <- getPlayerBet(player, playerId)
    } yield bet
  }

  def isTimeToSettle(game: BlackjackGameState): Boolean = {
    val handCount: Int = game.players.map(p => p.handsAndBets.map(_.hand)).length
    game.players != Nil && game.players.flatMap(p => p.handsAndBets.filter(h => h.wins.isDefined)).length == handCount
  }

  def isTimeToPlaceNewBets(game: BlackjackGameState): Boolean = {
    game.players.count(_.hands == Nil) == game.players.length
  }
  

  def getMinAndMaxBet(player: BlackjackPlayerState, game: BlackjackGameState): (Int, Int) =  {
    val minBet: Int = game.minimumBet * player.minBetMultiplier 
    val maxBet: Int = game.minimumBet * player.maxBetMultiplier match {
      case n if (n <= game.maximumBet) => n
      case _ => game.maximumBet
    }
    (minBet, maxBet)
  }

  // TODO: test
  def placeBet(game: BlackjackGameState): BlackjackGameState = {
    if (game.players == Nil)
      throw new IllegalArgumentException("Cannot place bet because there are no players")
    if (game.currentPlayerIndex.isEmpty) 
      throw new IllegalArgumentException("Cannot place bet because no player is designated as the current player")
    if (!isTimeToPlaceNewBets(game))
      throw new IllegalArgumentException("Cannot place new bets as it is not currently time to take new bets")

    val (minBet, maxBet): (Int, Int) = getMinAndMaxBet(game.currentPlayer(), game) 
    val amount: Int = {
      val numberOfLossesSinceLastWin: Int = 
        game
          .winningHistory(game.currentPlayer().id)
          .reverse // reverse to look from most recent to the oldest
          .takeWhile(!_) // false indicates loss
          .length

      val unrestrainedBet: Int = (game.currentPlayer().bettingStrategy, numberOfLossesSinceLastWin) match {
          case (NegativeProgression, 0) => minBet
          case (NegativeProgression, 1) => minBet * 2
          case (NegativeProgression, 2) => minBet * 2
          case (NegativeProgression, 3) => minBet * 3
          case (NegativeProgression, 4) => minBet * 3
          case (NegativeProgression, 5) => minBet * 4
          case (NegativeProgression, 6) => minBet * 4
          case (NegativeProgression, 7) => minBet * 5
          case (NegativeProgression, 8) => minBet * 10
          case (NegativeProgression, 9) => minBet * 25 
          case (NegativeProgression, _) => minBet * 50
          case (Martingale, lossCount) => lossCount match {
            case 0 => minBet
            case _ => (1 to lossCount).foldLeft(minBet)((acc, n) => acc * 2)
          }
          case (_, _) => ??? 
      }  
        val actualBet: Int = (unrestrainedBet, maxBet, game.currentPlayer().bank) match {
          case (u, max, bank) if (u > bank && bank <= max) => bank
          case (u, max, bank) if (u > bank && bank > max) => max
          case (u, max, _) if (u > max) => max
          case (u, _, _) => u 
        }
        actualBet
    }
    val updatedPlayers = game.players.map { p => 
      if (p.id == game.currentPlayer().id) 
        p.copy(handsAndBets = Seq(Hand(Nil, Map(p.id -> amount)))) 
      else 
        p
    }
    val updatedHistory = game.history ++ Seq(Action(game.currentPlayer().id, Bet, Nil, amount))
    game.copy(currentPlayerIndex = Some(game.nextPlayerIndex()), players = updatedPlayers, history = updatedHistory)
  }

  // adjusted payouts, for when player wins with blackjack or when dealer wins with a natural blackjack and player purchased insurance 
  //  1). for winning hands only: whether hand was a blackjack and which payout is in options (adjust to ratio for payout option)
  //  2). for dealer's natural blackjack win only: whether insurance was purchased, need to adjust it to pay 2-to-1 
  // returns: tuple with first item being the adjusted player states, and the 2nd item being the adjusted dealer's hand 
  private def adjustBetPayouts(players: Seq[BlackjackPlayerState], dealerHand: Hand, options: BlackjackOptions): (Seq[BlackjackPlayerState], Hand) = {
    val playerBlackjackWinAdjustedPayout: Hand => Hand = {
      val (numerator, denominator): (Int, Int) = options.payout match {
        case OneToOne => (1, 1)
        case ThreeToTwo => (3, 2) 
        case SixToFive => (6, 5) 
      }
      (hand: Hand) => Hand(hand.hand, hand.bets.map(b => if (evaluation.eval(hand.hand) == 21) b._1 -> b._2 * numerator / denominator else b).toMap, hand.wins) 
    }
    val dealerBlackjackWinAdjustedPayout: Hand => Hand = (dealerHand: Hand) => {
      if (evaluation.eval(dealerHand.hand) == 21 && dealerHand.hand.length == 2 && dealerHand.wins == Some(true))
        Hand(dealerHand.hand, dealerHand.bets.map(mapEntry => mapEntry._1 -> mapEntry._2 * 2), dealerHand.wins) // insurance pays 2-to-1
      else 
        dealerHand
    }
    (players.map(p => BlackjackPlayerState(p.id, p.bank, p.handsAndBets.map(h => playerBlackjackWinAdjustedPayout(h)) )), dealerBlackjackWinAdjustedPayout(dealerHand))
  }

  def settleBets(game: BlackjackGameState): BlackjackGameState = isTimeToSettle(game) match {
    case false => game
    case true => {
      def winningOrLosingWagers(players: Seq[BlackjackPlayerState], win: Boolean): Map[String, Int] = {
        val (adjustedWinningHandPayouts, adjustedInsurancePayouts): (Seq[BlackjackPlayerState], Hand) = adjustBetPayouts(players, game.dealerHand, game.options)
        (adjustedWinningHandPayouts.flatMap(_.handsAndBets.filter(_.wins == Some(win))) ++ Seq(adjustedInsurancePayouts).filter(_.wins == Some(win)))
          .flatMap(_.bets)
          .groupBy(_._1)
          .map(tup => (tup._1, tup._2.foldLeft(0)((acc, x) => x._2 + acc))) 
      }
      val winningWagers: Map[String, Int] = winningOrLosingWagers(game.players, true)
      val losingWagers: Map[String, Int] = winningOrLosingWagers(game.players, false)
      val wagers: Map[String, Int] = 
        (winningWagers.toSeq ++ losingWagers.toSeq.map(tup => (tup._1, -tup._2)))
          .groupBy(_._1)
          .map(tup => (tup._1, tup._2.foldLeft(0)((acc, x) => x._2 + acc)))

      val nextHistory: Seq[Action[BlackjackAction]] = game.history ++ wagers.toSeq.map(tup => {
        val action: BlackjackAction = if (tup._2 < 0) Lose else Win
        val amount: Int = tup._2.abs
        Action(tup._1, action, Nil, amount) 
      }).toSeq

      val updatedPlayers: Seq[BlackjackPlayerState] = game.players.map { p => 
        val updatedBank: Int = wagers.keySet.contains(p.id) match {
          case false => p.bank
          case true => p.bank + wagers(p.id)
        }
        BlackjackPlayerState(p.id, updatedBank, Nil)
      }  
      game.copy(players = updatedPlayers, history = nextHistory, dealerHand = Hand())
    } 
  }

}
