package cards.models.behaviors.evaluation

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.PokerHandEvaluation
import cards.models.behaviors.predicates.PokerPredicates
import cards.models.classes.{ Card, SuitedCard, Rank, Suit, PokerHandType }
import cards.models.classes.PokerHandType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class PokerHandEvaluationSpec extends AnyFlatSpec {
  private[evaluation] case object _commons extends Commons
  private[evaluation] case object _predicates extends PokerPredicates {
    override type CB = Commons
    override val commons = _commons
  }
  case object module extends PokerHandEvaluation {
    override type P = PokerPredicates 
    override val predicates = _predicates
  }

  "PokerHandEvaluation" should "score an empty hand as 0" in {
    val cards: Seq[Card] = Nil
    val result: Int = module.eval(cards)
    assert(result == 0)
  }

  it should "consider 2 empty hands as equals (with no preference)" in {
    val (cs1, cs2): (Seq[Card], Seq[Card]) = (Nil, Nil)
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    assert(result == None)
  }

  it should "prefer a 1 card hand over an empty hand" in {
    val (cs1, cs2): (Seq[Card], Seq[Card]) = (Nil, Seq(SuitedCard(Three, Diamonds)))
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    assert(result == Some(cs2))
  }

  it should "prefer whichever card has higher rank when comparing 2 single-carded hands" in {
    val (cs1, cs2): (Seq[Card], Seq[Card]) = (Seq(SuitedCard(Seven, Clubs)), Seq(SuitedCard(Three, Diamonds)))
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    assert(result == Some(cs1))
  }

  it should "prefer one pair (2 twos) to a hand with an ace high card" in {
    val highCard: Seq[Card] = Seq(SuitedCard(Nine, Diamonds), SuitedCard(Jack, Hearts), SuitedCard(Five, Clubs), SuitedCard(Seven, Spades), SuitedCard(Ace, Hearts))
    val onePair: Seq[Card] = Seq(SuitedCard(Two, Diamonds), SuitedCard(Two, Hearts), SuitedCard(Four, Clubs), SuitedCard(King, Spades), SuitedCard(Three, Hearts))
    assert(module.predicates.isHighCard(highCard))
    assert(module.predicates.isOnePair(onePair))
    val result: Option[Seq[Card]] = module.preference(highCard.sorted, onePair.sorted)
    assert(result == Some(onePair.sorted))
  }

  it should "prefer pair of threes over pair of twos, even when the pair of twos is accompanied by an Ace, a King, and a Queen and the pair of threeds is accompanied by the lowest cards" in {
    val pairOfTwos: Seq[Card] = Seq(SuitedCard(Two, Diamonds), SuitedCard(Two, Hearts), SuitedCard(Ace, Clubs), SuitedCard(King, Spades), SuitedCard(Queen, Hearts))
    val pairOfThrees: Seq[Card] = Seq(SuitedCard(Three, Diamonds), SuitedCard(Three, Hearts), SuitedCard(Two, Clubs), SuitedCard(Four, Spades), SuitedCard(Five, Hearts))
    assert(module.predicates.isOnePair(pairOfTwos))
    assert(module.predicates.isOnePair(pairOfThrees))
    val result: Option[Seq[Card]] = module.preference(pairOfTwos.sorted, pairOfThrees.sorted)
    assert(result == Some(pairOfThrees.sorted))
  }


}