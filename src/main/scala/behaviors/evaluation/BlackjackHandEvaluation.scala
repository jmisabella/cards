package cards.behaviors.evaluation

import cards.classes.Card
import cards.classes.hand.Hand
import cards.classes.state.BlackjackGameState
import cards.classes.Rank._
import cards.classes.Outcome._
import cards.behaviors.Commons
import scala.annotation.tailrec

trait BlackjackHandEvaluation extends HandEvaluation {
  type C <: Commons
  val commons: C 

  // if score exceeds 21 and there are one or more aces in the hand, reduce score by replacing values (as both allowed and neccessary)
  @tailrec
  private def reduce(score: Int, aces: Int): Int = (score, aces) match {
    case (x, 0) => x // no more aces, yield score
    case (x, _) if (x <= 21) => x // score <= 21, no need to reduce any further
    case (x, n) => reduce(x - 11 + 1, n - 1) // tail-recursive call to replace one of the aces' value of 11 by 2
  }
  override def eval(cards: Seq[Card]): Int = {
    // reduce score (e.g. 2 instead of 11 for ace) as necessary/allowed to drop below 22
    reduce(
      cards
        .map { c =>
          c.rank match {
            case Jack => 10
            case Queen => 10
            case King => 10
            case Ace => 11
            case r => commons.getNumeric(r)
          }
        }.foldLeft(0)(_ + _)
      , cards.count(_.rank == Ace)) // ace count
  }

  // TODO: test
  def outcomes(hand1: Hand, hand2: Hand): (Hand, Hand) = (eval(hand1.hand), eval(hand2.hand)) match {
    case (n1, n2) if (n1 > 21 && n2 > 21) => (hand1.copy(outcome = Some(Lose)), hand2.copy(outcome = Some(Lose)))
    case (n, _) if (n > 21) => (hand1.copy(outcome = Some(Lose)), hand2.copy(outcome = Some(Win)))
    case (_, n) if (n > 21) => (hand1.copy(outcome = Some(Win)), hand2.copy(outcome = Some(Lose)))
    case (n1, n2) if (n1 > n2) => (hand1.copy(outcome = Some(Win)), hand2.copy(outcome = Some(Lose)))
    case (n1, n2) if (n1 < n2) => (hand1.copy(outcome = Some(Lose)), hand2.copy(outcome = Some(Win)))
    case (_, _) => (hand1.copy(outcome = Some(Tie)), hand2.copy(outcome = Some(Tie)))
  }

  // TODO: test
  def outcomes(game: BlackjackGameState): BlackjackGameState = {
    val players = game.players.map { p =>
      val hands: Seq[Hand] = p.handsAndBets.map ( h => outcomes(h, game.dealerHand)._1)
      p.copy(handsAndBets = hands)
    }
    game.copy(players = players, currentPlayerIndex = None, currentHandIndex = None)
  }
}