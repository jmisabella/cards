package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.BlackJackPredicates
import cards.models.classes.{ Card, SuitedCard, UnsuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class BlackJackPredicatesSpec extends AnyFlatSpec {
  private[predicates] case object _commons extends Commons
  case object module extends BlackJackPredicates {
    override type CB = Commons
    override val commons = _commons
  }

  "BlackJackPredicates" should "not allow split for an empty hand" in {
    val cards: Seq[Card] = Nil
    val result: Boolean = module.canSplit(cards)
    assert(!result) 
  }

  it should "not allow split for a single-card hand" in {
    val cards: Seq[Card] = Seq(SuitedCard(Three, Clubs))
    val result: Boolean = module.canSplit(cards)
    assert(!result) 
  }

  it should "not allow split for a 2-card hand with cards of different rank" in {
    val cards: Seq[Card] = Seq(SuitedCard(Seven, Hearts), SuitedCard(Nine, Spades))
    val result: Boolean = module.canSplit(cards)
    assert(!result) 
  }

  it should "allow split for a 2-card hand with cards of matching rank" in {
    val cards: Seq[Card] = Seq(SuitedCard(Jack, Clubs), SuitedCard(Jack, Hearts))
    val result: Boolean = module.canSplit(cards)
    assert(result) 
  }
  
  it should "not allow split for a 3-card hand with 2 cards of matching rank and one additional card" in {
    val cards: Seq[Card] = Seq(SuitedCard(Jack, Clubs), SuitedCard(Jack, Hearts), SuitedCard(Two, Clubs))
    val result: Boolean = module.canSplit(cards)
    assert(!result) 
  }

  it should "not allow players insurance if dealer's face-up cards are an empty list" in {
    val dealerFaceUpCards: Seq[Card] = Nil
    val result: Boolean = module.eligibleForInstance(dealerFaceUpCards)
    assert(!result) 
  }
  
  it should "not allow players insurance if dealer's face-up cards have more than 1 card (an ace and one additional card)" in {
    val dealerFaceUpCards: Seq[Card] = Seq(SuitedCard(Ace, Spades), SuitedCard(King, Spades)) 
    val result: Boolean = module.eligibleForInstance(dealerFaceUpCards)
    assert(!result) 
  }

  it should "allow players insurance if dealer's face-up has only 1 card, an ace" in {
    val dealerFaceUpCards: Seq[Card] = Seq(SuitedCard(Ace, Hearts)) 
    val result: Boolean = module.eligibleForInstance(dealerFaceUpCards)
    assert(result) 
  }

}