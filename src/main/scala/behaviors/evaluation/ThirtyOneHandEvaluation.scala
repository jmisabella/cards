package cards.behaviors.evaluation

import cards.classes.Card
import cards.classes.Rank._
import cards.behaviors.Commons
import scala.annotation.tailrec

// Game play follows West Lansing Cut Throat rules
trait ThirtyOneHandEvaluation extends HandEvaluation {
  type C <: Commons
  val commons: C 

  override def eval(cards: Seq[Card]): Int = commons.suited(cards) match {
    // 3-of-a-kind is 30.5 points, so to simulate we're making it instead be 31, and actual 31 will then be scored as 32 (behind the scenes)
    case Nil => 0 
    case cs if (commons.countRank(cs).values.toSeq.contains(3)) => 31 // 3-of-a-kind, 30.5 points (see explanation above)
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

  // override preference so that if both hand evals tie, then hand with highest card by rank would break the tie (unless both highest ranks match)
  override def preference(cs1: Seq[Card], cs2: Seq[Card], jokerReplacement: Boolean = true): Option[Seq[Card]] = super.preference(cs1, cs2) match {
    case Some(results) => Some(results)
    case None => (cs1, cs2) match {
      case (Nil, Nil) => None
      case (Nil, _) => Some(cs2)
      case (_, Nil) => Some(cs1)
      // determine highest single card in either hand and use it to break the tie if possible
      case (_, _) => (commons.highest(cs1).head.rank.id.toInt, commons.highest(cs2).head.rank.id.toInt) match {
        case (c1, c2) if (c1 == c2) => None
        case (c1, c2) if (c1 > c2) => Some(cs1)
        case (c1, c2) if (c1 < c2) => Some(cs2)
      }
    }
  }
}