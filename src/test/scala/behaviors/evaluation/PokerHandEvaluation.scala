package cards.behaviors.evaluation

import cards.behaviors.Commons
import cards.behaviors.evaluation.PokerHandEvaluation
import cards.behaviors.predicates.PokerPredicates
import cards.classes.{ Card, Rank, Suit, PokerHandCategorization }
import cards.classes.PokerHandCategorization._
import cards.classes.Rank._
import cards.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

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
    val result: Long = module.eval(cards)
    result should equal (0)
  }

  it should "consider 2 empty hands as equals (with no preference)" in {
    val (cs1, cs2): (Seq[Card], Seq[Card]) = (Nil, Nil)
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    result shouldBe empty 
  }

  it should "prefer a 1 card hand over an empty hand" in {
    val (cs1, cs2): (Seq[Card], Seq[Card]) = (Nil, Seq(Card(Three, Diamonds)))
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    result should contain (cs2)
  }

  it should "prefer whichever card has higher rank when comparing 2 single-carded hands" in {
    val (cs1, cs2): (Seq[Card], Seq[Card]) = (Seq(Card(Seven, Clubs)), Seq(Card(Three, Diamonds)))
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    result should contain (cs1)
  }

  it should "prefer one pair (2 twos) to a hand with an ace high card" in {
    val highCard: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Jack, Hearts), Card(Five, Clubs), Card(Seven, Spades), Card(Ace, Hearts))
    val onePair: Seq[Card] = Seq(Card(Two, Diamonds), Card(Two, Hearts), Card(Four, Clubs), Card(King, Spades), Card(Three, Hearts))
    module.predicates.isHighCard(highCard) shouldBe (true) 
    module.predicates.isOnePair(onePair) shouldBe (true)
    val result: Option[Seq[Card]] = module.preference(highCard.sorted, onePair.sorted)
    result should contain (onePair.sorted) 
  }

  it should "prefer pair of threes over pair of twos, even when the pair of twos is accompanied by an Ace, a King, and a Queen and the pair of threeds is accompanied by the lowest cards" in {
    val pairOfTwos: Seq[Card] = Seq(Card(Two, Diamonds), Card(Two, Hearts), Card(Ace, Clubs), Card(King, Spades), Card(Queen, Hearts))
    val pairOfThrees: Seq[Card] = Seq(Card(Three, Diamonds), Card(Three, Hearts), Card(Two, Clubs), Card(Four, Spades), Card(Five, Hearts))
    module.predicates.isOnePair(pairOfTwos) shouldBe (true)
    module.predicates.isOnePair(pairOfThrees) shouldBe (true)
    val result: Option[Seq[Card]] = module.preference(pairOfTwos.sorted, pairOfThrees.sorted)
    result should contain (pairOfThrees.sorted) 
  }

  it should "prefer three of a kind (three twos with three and four) over hand with 2 pair (aces and kings) and a queen card" in {
    val twoPair: Seq[Card] = Seq(Card(Ace, Diamonds), Card(Ace, Hearts), Card(King, Clubs), Card(King, Spades), Card(Queen, Hearts))
    val threeOfAKind: Seq[Card] = Seq(Card(Two, Diamonds), Card(Two, Hearts), Card(Two, Clubs), Card(Three, Spades), Card(Four, Hearts))
    module.predicates.isTwoPair(twoPair) shouldBe (true)
    module.predicates.isThreeOfAKind(threeOfAKind) shouldBe (true)
    val result: Option[Seq[Card]] = module.preference(twoPair.sorted, threeOfAKind.sorted)
    result should contain (threeOfAKind.sorted) 
  }

  it should "prefer four of a kind (4 twos and a three) over a full house (three aces and 2 kings)" in {
    val fourOfAKind: Seq[Card] = Seq(Card(Two, Diamonds), Card(Two, Hearts), Card(Two, Clubs), Card(Two, Spades), Card(Three, Hearts))
    val fullHouse: Seq[Card] = Seq(Card(Ace, Diamonds), Card(Ace, Hearts), Card(Ace, Clubs), Card(King, Spades), Card(King, Hearts))
    module.predicates.isFourOfAKind(fourOfAKind) shouldBe (true)
    module.predicates.isFullHouse(fullHouse) shouldBe (true)
    val result: Option[Seq[Card]] = module.preference(fullHouse.sorted, fourOfAKind.sorted)
    result should contain (fourOfAKind.sorted)
  }

  it should "have no preference when both hands are straights and all ranks match between hands" in {
    val straightA: Seq[Card] = Seq(Card(Two, Diamonds), Card(Three, Hearts), Card(Four, Clubs), Card(Five, Spades), Card(Six, Hearts))
    val straightB: Seq[Card] = Seq(Card(Two, Clubs), Card(Three, Diamonds), Card(Four, Diamonds), Card(Five, Clubs), Card(Six, Clubs))
    module.predicates.isStraight(straightA) shouldBe (true)
    module.predicates.isStraight(straightB) shouldBe (true)
    val result: Option[Seq[Card]] = module.preference(straightA.sorted, straightB.sorted)
    info("no preference") 
    result shouldBe empty
  }

  it should "have no preference when both hands are royal flushes" in {
    val royalA: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Jack, Diamonds), Card(Queen, Diamonds), Card(King, Diamonds), Card(Ace, Diamonds))
    val royalB: Seq[Card] = Seq(Card(Ten, Clubs), Card(Jack, Clubs), Card(Queen, Clubs), Card(King, Clubs), Card(Ace, Clubs))
    module.predicates.isRoyalFlush(royalA) shouldBe (true)
    module.predicates.isRoyalFlush(royalB) shouldBe (true)
    val result: Option[Seq[Card]] = module.preference(royalA.sorted, royalB.sorted)
    info("no preference") 
    result shouldBe empty
  }

  it should "prefer highest card when there are no matches in either hand, but one high card is higher than the others" in {
    val highCardA: Seq[Card] = Seq(Card(Ace, Diamonds), Card(Two, Diamonds), Card(Three, Clubs), Card(Six, Spades), Card(Eight, Diamonds))
    val highCardB: Seq[Card] = Seq(Card(Ten, Clubs), Card(King, Clubs), Card(Queen, Spades), Card(Seven, Clubs), Card(Nine, Clubs))
    module.predicates.isHighCard(highCardA) shouldBe (true)
    module.predicates.isHighCard(highCardB) shouldBe (true)
    val result: Option[Seq[Card]] = module.preference(highCardA.sorted, highCardB.sorted)
    result should contain (highCardA.sorted) 
  }

  it should "have no preference between a Joker and an Ace" in {
    val joker: Seq[Card] = Seq(Card(LeftBower, Joker))
    val ace: Seq[Card] = Seq(Card(Ace, Spades))
    val result: Option[Seq[Card]] = module.preference(joker, ace)
    result shouldBe empty 
  }

  it should "prefer a Joker over an empty hand" in {
    val joker: Seq[Card] = Seq(Card(LeftBower, Joker))
    val empty: Seq[Card] = Nil
    val result: Option[Seq[Card]] = module.preference(joker, empty)
    result should contain (joker) 
  }

  it should "prefer pair of Twos, pair of Threes, and a Joker over a plain Flush" in {
    val fullHouseWithJoker: Seq[Card] = Seq(Card(RightBower, Joker), Card(Two, Clubs), Card(Two, Hearts), Card(Three, Spades), Card(Three, Clubs))
    val flush: Seq[Card] = Seq(Card(Five, Hearts), Card(Seven, Hearts), Card(Ten, Hearts), Card(Jack, Hearts), Card(Ace, Hearts))
    val result: Option[Seq[Card]] = module.preference(fullHouseWithJoker.sorted, flush.sorted)
    result should contain (fullHouseWithJoker.sorted)
  }

  it should "prefer three Twos, a Three, and a Joker over a Full House with 3 Aces and 2 Kings" in {
    val fourOfAKindWithJoker: Seq[Card] = Seq(Card(LeftBower, Joker), Card(Two, Clubs), Card(Two, Hearts), Card(Two, Spades), Card(Three, Clubs))
    val fullHouse: Seq[Card] = Seq(Card(Ace, Hearts), Card(Ace, Diamonds), Card(Ace, Clubs), Card(King, Hearts), Card(King, Spades))
    val result: Option[Seq[Card]] = module.preference(fourOfAKindWithJoker.sorted, fullHouse.sorted)
    result should contain (fourOfAKindWithJoker.sorted) 
  }

}