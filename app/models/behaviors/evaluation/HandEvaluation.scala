package cards.models.behaviors.evaluation

import cards.models.classes.Card

trait HandEvaluation {
  def eval(cards: Seq[Card]): Int
}