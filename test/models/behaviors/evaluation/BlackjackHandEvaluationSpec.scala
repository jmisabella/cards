package cards.models.behaviors.evaluation

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BlackJackHandEvaluationSpec extends AnyFlatSpec {
  private[evaluation] case object _commons extends Commons
  case object module extends BlackjackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }

  "BlackJackHandEvaluation" should "evaluate empty hand as 0" in {
    val cards: Seq[Card] = Nil
    val result: Int = module.eval(cards)
    result should equal (0)
  }

  it should "evaluate a single Ace as 11" in {
    val cards: Seq[Card] = Seq(Card(Ace, Spades))
    val result: Int = module.eval(cards)
    result should equal (11)
  }

  it should "evaluate two Aces as 12" in {
    val cards: Seq[Card] = Seq(Card(Ace, Diamonds), Card(Ace, Spades))
    val result: Int = module.eval(cards)
    result should equal (12)
  }

  it should "evaluate three Aces as 13" in {
    val cards: Seq[Card] = Seq(Card(Ace, Hearts), Card(Ace, Diamonds), Card(Ace, Spades))
    val result: Int = module.eval(cards)
    result should equal (13)
  }

  it should "evaluate four Aces as 14" in {
    val cards: Seq[Card] = Seq(Card(Ace, Clubs), Card(Ace, Hearts), Card(Ace, Diamonds), Card(Ace, Spades))
    val result: Int = module.eval(cards)
    result should equal (14)
  }

  it should "evaluate 21 Aces as 21" in {
    val cards: Seq[Card] = for (_ <- 0 until 21) yield Card(Ace, Diamonds)
    cards should have length (21) 
    val result: Int = module.eval(cards)
    result should equal (21)
  }

  it should "evaluate a single Two as 2" in {
    val cards: Seq[Card] = Seq(Card(Two, Clubs))
    val result: Int = module.eval(cards)
    result should equal (2)
  }

  it should "evaluate two Twos as 4" in {
    val cards: Seq[Card] = Seq(Card(Two, Hearts), Card(Two, Clubs))
    val result: Int = module.eval(cards)
    result should equal (4)
  }

  it should "evaluate 2 Jacks as 20" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Spades))
    val result: Int = module.eval(cards)
    result should equal (20)
  }

  it should "evaluate Queen and Ace as 21" in {
    val cards: Seq[Card] = Seq(Card(Jack, Spades), Card(Ace, Diamonds))
    val result: Int = module.eval(cards)
    result should equal (21)
  }

  it should "not have a hand preference between 2 hands which are both empty" in {
    val cs1: Seq[Card] = Nil
    val cs2: Seq[Card] = Nil
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    result should not be defined
    result shouldBe empty 
  }

  it should "not have a hand preference between 2 hands which match in rank" in {
    val cs1: Seq[Card] = Seq(Card(Two, Spades), Card(Five, Diamonds))
    val cs2: Seq[Card] = Seq(Card(Five, Hearts), Card(Two, Clubs))
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    result should not be defined
    result shouldBe empty 
  }

  it should "prefer a single card hand over an empty hand" in {
    val cs1: Seq[Card] = Seq(Card(Five, Diamonds))
    val cs2: Seq[Card] = Nil
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    result shouldBe defined
    result should contain (cs1) 
  }

}