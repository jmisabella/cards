package cards.models.behaviors.evaluation

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.BlackJackHandEvaluation
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class BlackJackHandEvaluationSpec extends AnyFlatSpec {
  private[evaluation] case object _commons extends Commons
  case object module extends BlackJackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }

  "BlackJackHandEvaluation" should "evaluate empty hand as 0" in {
    val cards: Seq[Card] = Nil
    val result: Int = module.eval(cards)
    assert(result == 0)
  }

  it should "evaluate a single Ace as 11" in {
    val cards: Seq[Card] = Seq(Card(Ace, Spades))
    val result: Int = module.eval(cards)
    assert(result == 11)
  }

  it should "evaluate two Aces as 12" in {
    val cards: Seq[Card] = Seq(Card(Ace, Diamonds), Card(Ace, Spades))
    val result: Int = module.eval(cards)
    assert(result == 12)
  }

  it should "evaluate three Aces as 13" in {
    val cards: Seq[Card] = Seq(Card(Ace, Hearts), Card(Ace, Diamonds), Card(Ace, Spades))
    val result: Int = module.eval(cards)
    assert(result == 13)
  }

  it should "evaluate four Aces as 14" in {
    val cards: Seq[Card] = Seq(Card(Ace, Clubs), Card(Ace, Hearts), Card(Ace, Diamonds), Card(Ace, Spades))
    val result: Int = module.eval(cards)
    assert(result == 14)
  }

  it should "evaluate 21 Aces as 21" in {
    val cards: Seq[Card] = for (_ <- 0 until 21) yield Card(Ace, Diamonds)
    assert(cards.length == 21)
    val result: Int = module.eval(cards)
    assert(result == 21)
  }

  it should "evaluate a single Two as 2" in {
    val cards: Seq[Card] = Seq(Card(Two, Clubs))
    val result: Int = module.eval(cards)
    assert(result == 2)
  }

  it should "evaluate two Twos as 4" in {
    val cards: Seq[Card] = Seq(Card(Two, Hearts), Card(Two, Clubs))
    val result: Int = module.eval(cards)
    assert(result == 4)
  }

  it should "evaluate 2 Jacks as 20" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Spades))
    val result: Int = module.eval(cards)
    assert(result == 20)
  }

  it should "evaluate Queen and Ace as 21" in {
    val cards: Seq[Card] = Seq(Card(Jack, Spades), Card(Ace, Diamonds))
    val result: Int = module.eval(cards)
    assert(result == 21)
  }

  it should "not have a hand preference between 2 hands which are both empty" in {
    val cs1: Seq[Card] = Nil
    val cs2: Seq[Card] = Nil
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    assert(!result.isDefined)
    assert(result.isEmpty)
  }

  it should "not have a hand preference between 2 hands which match in rank" in {
    val cs1: Seq[Card] = Seq(Card(Two, Spades), Card(Five, Diamonds))
    val cs2: Seq[Card] = Seq(Card(Five, Hearts), Card(Two, Clubs))
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    assert(!result.isDefined)
    assert(result.isEmpty)
  }

  it should "prefer a single card hand over an empty hand" in {
    val cs1: Seq[Card] = Seq(Card(Five, Diamonds))
    val cs2: Seq[Card] = Nil
    val result: Option[Seq[Card]] = module.preference(cs1, cs2)
    assert(result.isDefined)
    assert(result.get == cs1)
  }

}