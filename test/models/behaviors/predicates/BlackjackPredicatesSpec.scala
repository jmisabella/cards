package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.options.BlackjackOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackJackPredicatesSpec extends AnyFlatSpec with GivenWhenThen {
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

  it should "not allow split for a 2-card hand with cards of different rank whose values are both lower than 10" in {
    val cards: Seq[Card] = Seq(Card(Seven, Hearts), Card(Nine, Spades))
    val result: Boolean = module.canSplit(cards)
    result should be (false)
  }

  it should "allow split for a 2-card hand with cards of matching rank" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Hearts))
    val result: Boolean = module.canSplit(cards)
    result should be (true)
  }

  it should "by default allow split for a 2-card hand consisting of a Ten and a Queen" in {
    val cards: Seq[Card] = Seq(Card(Ten, Clubs), Card(Queen, Hearts))
    val result: Boolean = module.canSplit(cards)
    result should be (true)
  }
  
  it should ",with the splitOnRankMatchOnly option specified, not allow split for a 2-card hand consisting of a Ten and a Queen" in {
    val cards: Seq[Card] = Seq(Card(Ten, Clubs), Card(Queen, Hearts))
    val result: Boolean = module.canSplit(cards, BlackjackOptions(splitOnRankMatchOnly = true))
    result should be (false)
  }
  
  it should ",with the splitOnRankMatchOnly option specified, allow split for a 2-card hand consisting of two Tens" in {
    val cards: Seq[Card] = Seq(Card(Ten, Clubs), Card(Ten, Hearts))
    val result: Boolean = module.canSplit(cards, BlackjackOptions(splitOnRankMatchOnly = true))
    result should be (true)
  }

  it should "not allow split for a 3-card hand with 2 cards of matching rank and one additional card" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Hearts), Card(Two, Clubs))
    val result: Boolean = module.canSplit(cards)
    result should be (false)
  }

  it should "not allow players insurance if dealer's hand is an empty list" in {
    val dealerCards: Seq[Card] = Nil
    val result: Boolean = module.eligibleForInstance(dealerCards)
    result should be (false)
  }
  
  it should "not allow players insurance if dealer's hand has more than 1 card" in {
    val dealerCards: Seq[Card] = Seq(Card(Ace, Spades), Card(Seven, Hearts), Card(Three, Spades)) 
    val result: Boolean = module.eligibleForInstance(dealerCards)
    result should be (false)
  }

  it should "allow players insurance if dealer's hand has exactly 2 cards and its face-up card (first card) is an Ace" in {
    val dealerCards: Seq[Card] = Seq(Card(Ace, Hearts), Card(Two, Clubs))
    val result: Boolean = module.eligibleForInstance(dealerCards)
    result should be (true)
  }


}