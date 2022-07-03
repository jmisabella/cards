package cards.models.behaviors.evaluation

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.PokerHandEvaluation
import cards.models.behaviors.predicates.PokerPredicates
import cards.models.classes.{ Card, SuitedCard, UnsuitedCard, Rank, Suit, PokerHandType }
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

  it should "prefer three of a kind (three twos with three and four) over hand with 2 pair (aces and kings) and a queen card" in {
    val twoPair: Seq[Card] = Seq(SuitedCard(Ace, Diamonds), SuitedCard(Ace, Hearts), SuitedCard(King, Clubs), SuitedCard(King, Spades), SuitedCard(Queen, Hearts))
    val threeOfAKind: Seq[Card] = Seq(SuitedCard(Two, Diamonds), SuitedCard(Two, Hearts), SuitedCard(Two, Clubs), SuitedCard(Three, Spades), SuitedCard(Four, Hearts))
    assert(module.predicates.isTwoPair(twoPair))
    assert(module.predicates.isThreeOfAKind(threeOfAKind))
    val result: Option[Seq[Card]] = module.preference(twoPair.sorted, threeOfAKind.sorted)
    assert(result == Some(threeOfAKind.sorted))
  }

  it should "prefer four of a kind (4 twos and a three) over a full house (three aces and 2 kings)" in {
    val fourOfAKind: Seq[Card] = Seq(SuitedCard(Two, Diamonds), SuitedCard(Two, Hearts), SuitedCard(Two, Clubs), SuitedCard(Two, Spades), SuitedCard(Three, Hearts))
    val fullHouse: Seq[Card] = Seq(SuitedCard(Ace, Diamonds), SuitedCard(Ace, Hearts), SuitedCard(Ace, Clubs), SuitedCard(King, Spades), SuitedCard(King, Hearts))
    assert(module.predicates.isFourOfAKind(fourOfAKind))
    assert(module.predicates.isFullHouse(fullHouse))
    val result: Option[Seq[Card]] = module.preference(fullHouse.sorted, fourOfAKind.sorted)
    assert(result == Some(fourOfAKind.sorted))
  }

  it should "have no preference when both hands are straights and all ranks match between hands" in {
    val straightA: Seq[Card] = Seq(SuitedCard(Two, Diamonds), SuitedCard(Three, Hearts), SuitedCard(Four, Clubs), SuitedCard(Five, Spades), SuitedCard(Six, Hearts))
    val straightB: Seq[Card] = Seq(SuitedCard(Two, Clubs), SuitedCard(Three, Diamonds), SuitedCard(Four, Diamonds), SuitedCard(Five, Clubs), SuitedCard(Six, Clubs))
    assert(module.predicates.isStraight(straightA))
    assert(module.predicates.isStraight(straightB))
    val result: Option[Seq[Card]] = module.preference(straightA.sorted, straightB.sorted)
    assert(result == None) // no preference
  }

  it should "have no preference when both hands are royal flushes" in {
    val royalA: Seq[Card] = Seq(SuitedCard(Ten, Diamonds), SuitedCard(Jack, Diamonds), SuitedCard(Queen, Diamonds), SuitedCard(King, Diamonds), SuitedCard(Ace, Diamonds))
    val royalB: Seq[Card] = Seq(SuitedCard(Ten, Clubs), SuitedCard(Jack, Clubs), SuitedCard(Queen, Clubs), SuitedCard(King, Clubs), SuitedCard(Ace, Clubs))
    assert(module.predicates.isRoyalFlush(royalA))
    assert(module.predicates.isRoyalFlush(royalB))
    val result: Option[Seq[Card]] = module.preference(royalA.sorted, royalB.sorted)
    assert(result == None) // no preference
  }

  it should "prefer highest card when there are no matches in either hand, but one high card is higher than the others" in {
    val highCardA: Seq[Card] = Seq(SuitedCard(Ace, Diamonds), SuitedCard(Two, Diamonds), SuitedCard(Three, Clubs), SuitedCard(Six, Spades), SuitedCard(Eight, Diamonds))
    val highCardB: Seq[Card] = Seq(SuitedCard(Ten, Clubs), SuitedCard(King, Clubs), SuitedCard(Queen, Spades), SuitedCard(Seven, Clubs), SuitedCard(Nine, Clubs))
    assert(module.predicates.isHighCard(highCardA))
    assert(module.predicates.isHighCard(highCardB))
    val result: Option[Seq[Card]] = module.preference(highCardA.sorted, highCardB.sorted)
    assert(result == Some(highCardA.sorted))
  }

  it should "have no preference between a Joker and an Ace" in {
    val joker: Seq[Card] = Seq(UnsuitedCard(LeftBower))
    val ace: Seq[Card] = Seq(SuitedCard(Ace, Spades))
    val result: Option[Seq[Card]] = module.preference(joker, ace)
    assert(result == None)
  }

  it should "prefer a Joker over an empty hand" in {
    val joker: Seq[Card] = Seq(UnsuitedCard(LeftBower))
    val empty: Seq[Card] = Nil
    val result: Option[Seq[Card]] = module.preference(joker, empty)
    assert(result == Some(joker))
  }

  it should "prefer pair of Twos, pair of Threes, and a Joker over a plain Flush" in {
    val fullHouseWithJoker: Seq[Card] = Seq(UnsuitedCard(RightBower), SuitedCard(Two, Clubs), SuitedCard(Two, Hearts), SuitedCard(Three, Spades), SuitedCard(Three, Clubs))
    val flush: Seq[Card] = Seq(SuitedCard(Five, Hearts), SuitedCard(Seven, Hearts), SuitedCard(Ten, Hearts), SuitedCard(Jack, Hearts), SuitedCard(Ace, Hearts))
    val result: Option[Seq[Card]] = module.preference(fullHouseWithJoker.sorted, flush.sorted)
    assert(result == Some(fullHouseWithJoker.sorted))
  }

  it should "prefer three Twos, a Three, and a Joker over a Full House with 3 Aces and 2 Kings" in {
    val fourOfAKindWithJoker: Seq[Card] = Seq(UnsuitedCard(LeftBower), SuitedCard(Two, Clubs), SuitedCard(Two, Hearts), SuitedCard(Two, Spades), SuitedCard(Three, Clubs))
    val fullHouse: Seq[Card] = Seq(SuitedCard(Ace, Hearts), SuitedCard(Ace, Diamonds), SuitedCard(Ace, Clubs), SuitedCard(King, Hearts), SuitedCard(King, Spades))
    val result: Option[Seq[Card]] = module.preference(fourOfAKindWithJoker.sorted, fullHouse.sorted)
    assert(result == Some(fourOfAKindWithJoker.sorted))
  }

}