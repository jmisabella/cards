package cards.models.behaviors.evaluation

import cards.models.classes.Card

trait HandEvaluation {
  def eval(cards: Seq[Card]): Int

  // TODO: test
  // preference: given 2 hands yield which is better, unless both hands evaluate to the same score 
  def preference(cs1: Seq[Card], cs2: Seq[Card]): Option[Seq[Card]] = (eval(cs1), eval(cs2)) match {
    case (x, y) if (x < y) => Some(cs2)
    case (x, y) if (x > y) => Some(cs1)
    case (_, _) => None // scores match, no preference
  }
}