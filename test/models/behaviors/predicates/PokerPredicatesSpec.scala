package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.PokerPredicates
import cards.models.classes.{ Card, SuitedCard, UnsuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class PokerPredicatesSpec extends AnyFlatSpec {

  private[predicates] case object _commons extends Commons
  case object module extends PokerPredicates {
    override type CB = Commons
    override val commons = _commons
  }

  "PokerPredicates" should "observe a royal flush" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Clubs), 
      SuitedCard(Jack, Clubs), 
      SuitedCard(Queen, Clubs), 
      SuitedCard(King, Clubs),
      SuitedCard(Ace, Clubs))

    val result = module.isRoyalFlush(cards)
    assert(result)
  }

  it should "observe when it's not a royal flush" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Jack, Clubs), 
      SuitedCard(Queen, Diamonds), 
      SuitedCard(King, Clubs),
      SuitedCard(Ace, Clubs))

    val result = module.isRoyalFlush(cards)
    assert(!result)
  }
  
  it should "observe when it's a flush" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Hearts), 
      SuitedCard(Queen, Hearts), 
      SuitedCard(Seven, Hearts),
      SuitedCard(Ace, Hearts))

    val result = module.isFlush(cards)
    assert(result)
  }
  
  it should "observe when it's a straight" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Jack, Clubs), 
      SuitedCard(Queen, Diamonds), 
      SuitedCard(King, Clubs),
      SuitedCard(Ace, Clubs))

    val result = module.isStraight(cards)
    assert(result)
  }

  it should "observe when it's a high card" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Queen, Diamonds), 
      SuitedCard(Ace, Hearts),
      SuitedCard(Five, Clubs))

    val result = module.isHighCard(cards)
    assert(result)
  }

  it should "yield no high card when there is one pair" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Ace, Hearts),
      SuitedCard(Five, Clubs))

    assert(!module.isHighCard(cards))
    assert(module.isOnePair(cards))
    val result: Option[Card] = module.highCard(cards)
    assert(!result.isDefined)
  }

  it should "yield high card when there is only a high card and no other matches in the hand" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Four, Diamonds), 
      SuitedCard(Ace, Hearts),
      SuitedCard(Five, Clubs))

    assert(module.isHighCard(cards))
    val result: Option[Card] = module.highCard(cards)
    assert(result.isDefined)
    assert(result.get == SuitedCard(Ace, Hearts))
  }

  it should "yield nothing for onePair when isOnePair is false" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Seven, Diamonds), 
      SuitedCard(Ace, Hearts),
      SuitedCard(Five, Clubs))

    assert(!module.isOnePair(cards))
    val result: Option[Seq[Card]] = module.onePair(cards)
    assert(!result.isDefined)
  }
  
  it should "yield 1 pair when onePair is true and there is exactly 1 pair" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Ace, Hearts),
      SuitedCard(Five, Clubs))

    assert(module.isOnePair(cards))
    val result: Option[Seq[Card]] = module.onePair(cards)
    assert(result.isDefined)
    assert(result.get.length == 2)
    assert(result == Some(Seq(SuitedCard(Ten, Hearts), SuitedCard(Ten, Diamonds))))
  }
  
  it should "yield 2 pairs for isTwoPair and there are exactly 2 pairs" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Five, Clubs))

    assert(!module.isHighCard(cards))
    assert(!module.isOnePair(cards))
    assert(module.isTwoPair(cards))
    val result: Option[Seq[Card]] = module.twoPair(cards)
    assert(result.isDefined)
    assert(result.get.length == 4)
    assert(result.get.count(c => Seq(SuitedCard(Ten, Hearts), SuitedCard(Ten, Diamonds), SuitedCard(Two, Clubs), SuitedCard(Two, Hearts)).contains(c)) == result.get.length)
  }

  it should "not yield 3 of a kind when isThreeOfAKind is false" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Five, Clubs))

    assert(!module.isOnePair(cards))
    assert(module.isTwoPair(cards))
    assert(!module.isThreeOfAKind(cards))
    val result: Option[Seq[Card]] = module.threeOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "yield 3 of a kind for isThreeOfAKind there are 3 of a kind" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Ten, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Five, Clubs))

    assert(!module.isTwoPair(cards))
    assert(module.isThreeOfAKind(cards))
    val result: Option[Seq[Card]] = module.threeOfAKind(cards)
    assert(result.isDefined)
    assert(result.get.length == 3)
    assert(result.get.count(c => Seq(SuitedCard(Ten, Hearts), SuitedCard(Ten, Diamonds), SuitedCard(Ten, Clubs)).contains(c)) == result.get.length)
  }

  it should "not yield 4 of a kind when isFourOfAKind is false, with 2 pairs existing" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Five, Clubs))

    assert(!module.isOnePair(cards))
    assert(module.isTwoPair(cards))
    assert(!module.isFourOfAKind(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "not yield 4 of a kind when isFourOfAKind is false, with 3 of a kind" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Ten, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Five, Clubs))

    assert(module.isThreeOfAKind(cards))
    assert(!module.isFourOfAKind(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "yield 4 of a kind when isFourOfAKind and 4 of a kind exists" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Ten, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Ten, Spades))

    assert(!module.isThreeOfAKind(cards))
    assert(module.isFourOfAKind(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(result.isDefined)
    assert(result.get.length == 4)
    assert(result.get.count(c => Seq(SuitedCard(Ten, Hearts), SuitedCard(Ten, Diamonds), SuitedCard(Ten, Clubs), SuitedCard(Ten, Spades)).contains(c)) == result.get.length)
  }

  it should "not yield full house when isFullHouse is false (with 2 pair present)" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Ten, Clubs), 
      SuitedCard(Two, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Five, Clubs))

    assert(module.isTwoPair(cards))
    assert(!module.isFullHouse(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "not yield full house when isFullHouse is false (with 3 of a kind present)" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Ten, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Five, Clubs))

    assert(module.isThreeOfAKind(cards))
    assert(!module.isFullHouse(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }
  
  it should "yield full house when isFullHouse (with 3 of a kind and 1 pair present)" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Ten, Clubs), 
      SuitedCard(Ten, Diamonds), 
      SuitedCard(Two, Hearts),
      SuitedCard(Two, Clubs))

    assert(module.isThreeOfAKind(cards))
    assert(module.isOnePair(cards))
    assert(module.isFullHouse(cards))
    val result: Option[Seq[Card]] = module.fullHouse(cards)
    assert(result.isDefined)
  }

}