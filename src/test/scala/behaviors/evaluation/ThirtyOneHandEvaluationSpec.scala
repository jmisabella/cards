package cards.behaviors.evaluation

import cards.behaviors.Commons
import cards.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.classes.{ Card, Rank, Suit }
import cards.classes.Rank._
import cards.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ThirtyOneHandEvaluationSpec extends AnyFlatSpec {
  private[evaluation] case object _commons extends Commons
  case object module extends ThirtyOneHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }

  "ThirtyOneHandEvaluation" should "evaluate an empty hand as 0" in {
    val cards: Seq[Card] = Nil
    val result: Long = module.eval(cards)
    result should equal (0)
  }

  it should "evaluate a single Two as 2" in {
    val cards: Seq[Card] = Seq(Card(Two, Hearts))
    val result: Long = module.eval(cards)
    result should equal (2)
  } 
  
  it should "evaluate a single Ace as 11" in {
    val cards: Seq[Card] = Seq(Card(Ace, Diamonds))
    val result: Long = module.eval(cards)
    result should equal (11)
  } 
  
  it should "evaluate a Jack and Ace of different suits as 11" in {
    val cards: Seq[Card] = Seq(Card(Ace, Diamonds), Card(Jack, Clubs))
    val result: Long = module.eval(cards)
    result should equal (11)
  } 

  it should "evaluate a Jack and Ace of same suits as 21" in {
    val cards: Seq[Card] = Seq(Card(Ace, Spades), Card(Jack, Spades))
    val result: Long = module.eval(cards)
    result should equal (21)
  } 

  it should "evaluate a Ten, Jack and Ace of same suits as 32" in {
    val cards: Seq[Card] = Seq(Card(Ten, Spades), Card(Ace, Spades), Card(Jack, Spades))
    val result: Long = module.eval(cards)
    result should equal (32)
  } 
  
  it should "evaluate two Jacks of different suits as 10" in {
    val cards: Seq[Card] = Seq(Card(Jack, Hearts), Card(Jack, Spades))
    val result: Long = module.eval(cards)
    result should equal (10)
  } 

  it should "evaluate Jack, Queen, and King of same suits as 30" in {
    val cards: Seq[Card] = Seq(Card(Jack, Hearts), Card(Queen, Hearts), Card(King, Hearts))
    val result: Long = module.eval(cards)
    result should equal (30)
  } 

  it should "evaluate 3 Jacks from different suits as 31 (simulating 30.5)" in {
    val cards: Seq[Card] = Seq(Card(Jack, Hearts), Card(Jack, Clubs), Card(Jack, Diamonds))
    val result: Long = module.eval(cards)
    result should equal (31)
  } 

  it should "evaluate 3 Twos from different suits as 31 (simulating 30.5)" in {
    val cards: Seq[Card] = Seq(Card(Two, Hearts), Card(Two, Clubs), Card(Two, Diamonds))
    val result: Long = module.eval(cards)
    result should equal (31)
  } 

  it should "break a tie by using high card, v1" in {
    val cards1: Seq[Card] = Seq(Card(Two, Diamonds), Card(Six, Diamonds), Card(King, Diamonds))
    val cards2: Seq[Card] = Seq(Card(Seven, Spades), Card(Three, Spades), Card(Eight, Spades))
    val preferred: Option[Seq[Card]] = module.preference(cards1, cards2)
    preferred shouldBe defined
    preferred.get.sorted should equal (cards1.sorted)
  }

  it should "break a tie by using high card, v2" in {
    val cards1: Seq[Card] = Seq(Card(Two, Diamonds), Card(Six, Diamonds), Card(Jack, Diamonds))
    val cards2: Seq[Card] = Seq(Card(Four, Spades), Card(Four, Spades), Card(King, Spades))
    val preferred: Option[Seq[Card]] = module.preference(cards1, cards2)
    preferred shouldBe defined 
    preferred.get.sorted should equal (cards2.sorted)
  }

  it should "not break a tie when both hands share the same high card rank" in {
    val cards1: Seq[Card] = Seq(Card(Two, Diamonds), Card(Queen, Diamonds), Card(Ten, Diamonds))
    val cards2: Seq[Card] = Seq(Card(Two, Spades), Card(Jack, Spades), Card(Queen, Spades))
    val preferred: Option[Seq[Card]] = module.preference(cards1, cards2)
    preferred shouldBe empty
    preferred should not be defined 
  }

  it should "use permutations and scores to determine the best 3-card hand from 4 cards" in {
    val cards: Seq[Card] = Seq(Card(Two, Hearts), Card(Queen, Diamonds), Card(Ace, Spades), Card(Five, Hearts))
    val highestHand = module.permutationsAndScores(cards, 3).maxBy(_._2)._1
    val highestScore = module.permutationsAndScores(cards, 3).maxBy(_._2)._2
    highestHand should contain (Card(Ace, Spades))
    highestScore should equal (11) 
  }

  it should "throw illegal state exception when attempting permutations and scores when n (5) is larger than the hand size (4)" in {
    val cards: Seq[Card] = Seq(Card(Two, Hearts), Card(Queen, Diamonds), Card(Ace, Spades), Card(Five, Hearts))
    an [IllegalStateException] should be thrownBy module.permutationsAndScores(cards, 5).maxBy(_._2)._1
    an [IllegalStateException] should be thrownBy module.permutationsAndScores(cards, 5).maxBy(_._2)._2
  }
}