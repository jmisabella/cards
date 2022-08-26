package cards.models.classes.state

import cards.models.classes.state.{ PlayerState, GameState }
import cards.models.classes.{ Card, Rank, Suit, Deck, HandBet }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.options.Surrender._
import cards.models.classes.options.DealerHitLimit._
import cards.models.classes.options.ResplitLimit._
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._

case class BlackjackPlayerState(id: String, bank: Int = 0, handsAndBets: Seq[HandBet] = Nil) extends PlayerState {
  val hands: Seq[Seq[Card]] = handsAndBets.map(_.hand)
  def playerBet(playerId: String): Option[(Seq[Card], Int)] = { 
    (for {
      x <- handsAndBets
      if (x.bets.keys.toSeq.contains(playerId))
    } yield (x.hand, x.bets.filter(_._1 == playerId).values.head)
    ).headOption
  }
}

object BlackjackPlayerState {
  def apply(hands: Seq[Seq[Card]], id: String, bank: Int): BlackjackPlayerState = BlackjackPlayerState(id, bank, hands.map(h => HandBet(h)))
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
  dealerHand: Seq[Card] = Nil,
  minimumBet: Int = 1,
  maximumBet: Int = 999999) extends GameState[BlackjackPlayerState, BlackjackAction] {

    def playerBets(playerId: String): Seq[(Seq[Card], Int)] = {
      for {
        player <- players
        bet <- player.playerBet(playerId)
      } yield bet
    }

    def isTimeToSettle(): Boolean = {
      val handCount: Int = players.map(p => p.handsAndBets.map(_.hand)).length
      players != Nil && players.flatMap(p => p.handsAndBets.filter(h => h.handWins.isDefined)).length == handCount
    }

    def settleBets(): BlackjackGameState = isTimeToSettle() match {
      case false => this
      case true => {
        def winningOrLosingWagers(players: Seq[BlackjackPlayerState], win: Boolean): Map[String, Int] = players
          .flatMap(_.handsAndBets.filter(_.handWins == Some(win)))
          .flatMap(_.bets)
          .groupBy(_._1)
          .map(tup => (tup._1, tup._2.foldLeft(0)((acc, x) => x._2 + acc))) 

        val winningWagers: Map[String, Int] = winningOrLosingWagers(players, true)
        val losingWagers: Map[String, Int] = winningOrLosingWagers(players, false)
        val wagers: Map[String, Int] = 
          (winningWagers.toSeq ++ losingWagers.toSeq.map(tup => (tup._1, -tup._2)))
            .groupBy(_._1)
            .map(tup => (tup._1, tup._2.foldLeft(0)((acc, x) => x._2 + acc)))

        val nextHistory: Seq[Action[BlackjackAction]] = this.history ++ wagers.toSeq.map(tup => {
          val action: BlackjackAction = if (tup._2 < 0) Lose else Win
          val amount: Int = tup._2.abs
          Action(tup._1, action, Nil, amount) 
        }).toSeq

        val updatedPlayers: Seq[BlackjackPlayerState] = players.map { p => 
          val updatedBank: Int = wagers.keySet.contains(p.id) match {
            case false => p.bank
            case true => p.bank + wagers(p.id)
          }
          BlackjackPlayerState(p.id, updatedBank, Nil)
        }  
        copy(players = updatedPlayers, history = nextHistory, dealerHand = Nil)
      } 
    }

}
