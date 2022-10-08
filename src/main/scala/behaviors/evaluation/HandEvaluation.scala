package cards.behaviors.evaluation

import cards.classes.{ Card, Deck }
import cards.classes.DeckType._

trait HandEvaluation {

  // evaluate given hand as an integer score 
  def eval(cards: Seq[Card]): Int

  // transform hand into the best possible hand with jokers replaced by suited cards 
  def jokerWildcardReplacement(cards: Seq[Card]): Seq[Card] = cards.count(_.isJoker) match {
    case 0 => cards // no jokers, nothing to replace
    case n => {
      // all combinations of replacements for however many joker cards exist
      val replacements: List[List[Card]] = Deck(JokersExcluded).cards.combinations(n).toList

      // yield each possibility as well as its evaluated integer score
      val possibilities: List[(List[Card], Int)] = replacements.map { cs => 
        val possibility: List[Card] = cs ++ cards.filter(!_.isJoker)
        (possibility, eval(possibility))
      }
      possibilities.maxBy(_._2)._1 // take the max hand; arbitrarily choose in case of multiple ties at max score
    }
  }

  // preference: given 2 hands yield which is better, unless both hands evaluate to the same score 
  def preference(cs1: Seq[Card], cs2: Seq[Card], jokerReplacement: Boolean = true): Option[Seq[Card]] = {
    val (score1, score2): (Int, Int) = jokerReplacement match {
      case false => (eval(cs1), eval(cs2))
      case true => (eval(jokerWildcardReplacement(cs1)), eval(jokerWildcardReplacement(cs2)))
    }
    (score1, score2) match {
      case (x, y) if (x < y) => Some(cs2)
      case (x, y) if (x > y) => Some(cs1)
      case (_, _) => None // scores match, no preference
    }
  }

  // Given a collection of cards and n cards per hand, yield possible hand permutations and evaluated scores
  def permutationsAndScores(cards: Seq[Card], n: Int): Seq[(Seq[Card], Int)] = {
    if (n > cards.length)
      throw new IllegalStateException(s"Cannot get permutations when n is [$n] which exceeds list length [${cards.length}]")
    
    cards 
      .combinations(n) // every permutation of n cards
      .map(cs => (cs, eval(cs)))
      .toSeq
  }

}