package cards.models.behaviors.evaluation

import cards.models.classes.{ Card, SuitedCard }
import cards.models.classes.Rank._
import cards.models.behaviors.Commons
import scala.annotation.tailrec

trait ThirtyOneHandEvaluation extends HandEvaluation {
  type C <: Commons
  val commons: C 

  override def eval(cards: Seq[Card]): Int = commons.suited(cards) match {
    // 3-of-a-kind is 30.5 points, so to simulate we're making this 31, and actual 31 will then be scored as 32
    case Nil => 0 
    case cs if (commons.countRank(cs).values.toSeq.contains(3)) => 31 // 3-of-a-kind
    case cs => { // else not 3-of-a-kind, so determine highest score grouped by suit
      val cardEval = (rank: Rank) => rank match {
        case Jack => 10
        case Queen => 10
        case King => 10
        case Ace => 11
        case r => commons.getNumeric(r)
      }
      cs.groupBy(_.suit) // group by suit, sum up each grouping's card values, take the max
        .map(kv => kv._1 -> kv._2.foldLeft(0)((acc, c1) => acc + cardEval(c1.rank)))
        .values
        .max match {
          case 31 => 32 // since 3-of-a-kind's 30.5 is rounded up to 31, 31 now becomes 32
          case x => x // else no special logic if it's not 31
        }
    }
  }
}