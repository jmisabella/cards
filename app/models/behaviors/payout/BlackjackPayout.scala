package cards.models.behaviors.payout

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
import cards.models.behaviors.evaluation.BlackjackHandEvaluation

trait BlackjackPayout {
  type EVAL <: BlackjackHandEvaluation 
  val evaluation: EVAL

  def playerBet(playerState: BlackjackPlayerState, playerId: String): Option[(Seq[Card], Int)] = { 
    (for {
      x <- playerState.handsAndBets
      if (x.bets.keys.toSeq.contains(playerId))
    } yield (x.hand, x.bets.filter(_._1 == playerId).values.head)
    ).headOption
  }

  def playerBets(game: BlackjackGameState, playerId: String): Seq[(Seq[Card], Int)] = {
    for {
      player <- game.players
      bet <- playerBet(player, playerId)
    } yield bet
  }

  def isTimeToSettle(game: BlackjackGameState): Boolean = {
    val handCount: Int = game.players.map(p => p.handsAndBets.map(_.hand)).length
    game.players != Nil && game.players.flatMap(p => p.handsAndBets.filter(h => h.wins.isDefined)).length == handCount
  }


  def settleBets(game: BlackjackGameState): BlackjackGameState = isTimeToSettle(game) match {
    case false => game
    case true => {
      def winningOrLosingWagers(players: Seq[BlackjackPlayerState], win: Boolean): Map[String, Int] = players
        .flatMap(_.handsAndBets.filter(_.wins == Some(win)))
        .flatMap(_.bets)
        .groupBy(_._1)
        .map(tup => (tup._1, tup._2.foldLeft(0)((acc, x) => x._2 + acc))) 

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
      game.copy(players = updatedPlayers, history = nextHistory, dealerHand = Nil)
    } 
  }

}
