package cards.behaviors.play

// import cards.behaviors.predicates.PokerPredicates
import cards.behaviors.evaluation.PokerHandEvaluation
import cards.classes.hand.Hand
import cards.classes.Card

trait PokerPlay {

  type EVAL <: PokerHandEvaluation
  val evaluation: EVAL

  // Given collection of Hands and a player's identifier (name), yield the best 5-card hand of the cards belonging to the player
  def cards(hands: Seq[Hand], playerId: String): Seq[Card] = {
    hands
      .filter(_.owners.contains(playerId))
      .flatMap(_.hand) match {
        // of cards belonging to player, length is 5 or less so simply yield all of the player's cards
        case cs if (cs.length <= 5) => cs
        // else more than 5 cards belong to player, so we must choose the best 5-card hand of the player's total cards
        case cs => cs.combinations(5).maxBy(xs => evaluation.eval(xs))
      }
  }

}
