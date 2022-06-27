package cards.models.behaviors.evaluation

import cards.models.classes.{ Card, SuitedCard }
import cards.models.classes.Rank._
import cards.models.behaviors.Commons
import scala.annotation.tailrec

trait BlackJackHandEvaluation extends HandEvaluation {
  type C <: Commons
  val commons: C 
  
  @tailrec
  private def reduce(score: Int, aces: Int): Int = (score, aces) match {
    case (x, 0) => x // no more aces, yield score
    case (x, _) if (x <= 21) => x // score <= 21, no need to reduce any further
    case (x, n) => reduce(x - 11 + 1, n - 1)
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
}