package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BlackJackPredicatesSpec extends AnyFlatSpec {
  private[predicates] case object _commons extends Commons
  case object module extends BlackjackPredicates {
    override type CB = Commons
    override val commons = _commons
  }

  "BlackJackPredicates" should "not allow split for an empty hand" in {
    val cards: Seq[Card] = Nil
    val result: Boolean = module.canSplit(cards)
    result should be (false)
  }

  it should "not allow split for a single-card hand" in {
    val cards: Seq[Card] = Seq(Card(Three, Clubs))
    val result: Boolean = module.canSplit(cards)
    result should be (false)
  }

  it should "not allow split for a 2-card hand with cards of different rank" in {
    val cards: Seq[Card] = Seq(Card(Seven, Hearts), Card(Nine, Spades))
    val result: Boolean = module.canSplit(cards)
    result should be (false)
  }

  it should "allow split for a 2-card hand with cards of matching rank" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Hearts))
    val result: Boolean = module.canSplit(cards)
    result should be (true)
  }
  
  it should "not allow split for a 3-card hand with 2 cards of matching rank and one additional card" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Hearts), Card(Two, Clubs))
    val result: Boolean = module.canSplit(cards)
    result should be (false)
  }

  it should "not allow players insurance if dealer's face-up cards are an empty list" in {
    val dealerFaceUpCards: Seq[Card] = Nil
    val result: Boolean = module.eligibleForInstance(dealerFaceUpCards)
    result should be (false)
  }
  
  it should "not allow players insurance if dealer's face-up cards have more than 1 card (an ace and one additional card)" in {
    val dealerFaceUpCards: Seq[Card] = Seq(Card(Ace, Spades), Card(King, Spades)) 
    val result: Boolean = module.eligibleForInstance(dealerFaceUpCards)
    result should be (false)
  }

  it should "allow players insurance if dealer's face-up has only 1 card, an ace" in {
    val dealerFaceUpCards: Seq[Card] = Seq(Card(Ace, Hearts)) 
    val result: Boolean = module.eligibleForInstance(dealerFaceUpCards)
    result should be (true)
  }

}