package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.PokerPredicates
import cards.models.classes.{ Card, Rank, Suit, PokerHandType }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.PokerHandType._
import org.scalatest.flatspec.AnyFlatSpec

class PokerPredicatesSpec extends AnyFlatSpec {

  private[predicates] case object _commons extends Commons
  case object module extends PokerPredicates {
    override type CB = Commons
    override val commons = _commons
  }

  "PokerPredicates" should "observe a royal flush" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Clubs), 
      Card(Jack, Clubs), 
      Card(Queen, Clubs), 
      Card(King, Clubs),
      Card(Ace, Clubs))

    val result = module.isRoyalFlush(cards)
    assert(result)
  }

  it should "observe when it's not a royal flush" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Jack, Clubs), 
      Card(Queen, Diamonds), 
      Card(King, Clubs),
      Card(Ace, Clubs))

    val result = module.isRoyalFlush(cards)
    assert(!result)
  }
  
  it should "observe when it's a flush" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Hearts), 
      Card(Queen, Hearts), 
      Card(Seven, Hearts),
      Card(Ace, Hearts))

    val result = module.isFlush(cards)
    assert(result)
  }
  
  it should "observe when it's a straight" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Jack, Clubs), 
      Card(Queen, Diamonds), 
      Card(King, Clubs),
      Card(Ace, Clubs))

    val result = module.isStraight(cards)
    assert(result)
  }

  it should "observe when it's a high card" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Queen, Diamonds), 
      Card(Ace, Hearts),
      Card(Five, Clubs))

    val result = module.isHighCard(cards)
    assert(result)
  }

  it should "yield no high card when there is one pair" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Ten, Diamonds), 
      Card(Ace, Hearts),
      Card(Five, Clubs))

    assert(!module.isHighCard(cards))
    assert(module.isOnePair(cards))
    val result: Option[Card] = module.highCard(cards)
    assert(!result.isDefined)
  }

  it should "yield high card when there is only a high card and no other matches in the hand" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Four, Diamonds), 
      Card(Ace, Hearts),
      Card(Five, Clubs))

    assert(module.isHighCard(cards))
    val result: Option[Card] = module.highCard(cards)
    assert(result.isDefined)
    assert(result.get == Card(Ace, Hearts))
  }

  it should "yield nothing for onePair when isOnePair is false" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Seven, Diamonds), 
      Card(Ace, Hearts),
      Card(Five, Clubs))

    assert(!module.isOnePair(cards))
    val result: Option[Seq[Card]] = module.onePair(cards)
    assert(!result.isDefined)
  }
  
  it should "yield 1 pair when onePair is true and there is exactly 1 pair" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Ten, Diamonds), 
      Card(Ace, Hearts),
      Card(Five, Clubs))

    assert(module.isOnePair(cards))
    val result: Option[Seq[Card]] = module.onePair(cards)
    assert(result.isDefined)
    assert(result.get.length == 2)
    assert(result == Some(Seq(Card(Ten, Hearts), Card(Ten, Diamonds))))
  }
  
  it should "yield 2 pairs for isTwoPair and there are exactly 2 pairs" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Five, Clubs))

    assert(!module.isHighCard(cards))
    assert(!module.isOnePair(cards))
    assert(module.isTwoPair(cards))
    val result: Option[Seq[Card]] = module.twoPair(cards)
    assert(result.isDefined)
    assert(result.get.length == 4)
    assert(result.get.count(c => Seq(Card(Ten, Hearts), Card(Ten, Diamonds), Card(Two, Clubs), Card(Two, Hearts)).contains(c)) == result.get.length)
  }

  it should "not yield 3 of a kind when isThreeOfAKind is false" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Five, Clubs))

    assert(!module.isOnePair(cards))
    assert(module.isTwoPair(cards))
    assert(!module.isThreeOfAKind(cards))
    val result: Option[Seq[Card]] = module.threeOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "yield 3 of a kind for isThreeOfAKind there are 3 of a kind" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Ten, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Five, Clubs))

    assert(!module.isTwoPair(cards))
    assert(module.isThreeOfAKind(cards))
    val result: Option[Seq[Card]] = module.threeOfAKind(cards)
    assert(result.isDefined)
    assert(result.get.length == 3)
    assert(result.get.count(c => Seq(Card(Ten, Hearts), Card(Ten, Diamonds), Card(Ten, Clubs)).contains(c)) == result.get.length)
  }

  it should "not yield 4 of a kind when isFourOfAKind is false, with 2 pairs existing" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Two, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Five, Clubs))

    assert(!module.isOnePair(cards))
    assert(module.isTwoPair(cards))
    assert(!module.isFourOfAKind(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "not yield 4 of a kind when isFourOfAKind is false, with 3 of a kind" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Ten, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Five, Clubs))

    assert(module.isThreeOfAKind(cards))
    assert(!module.isFourOfAKind(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "yield 4 of a kind when isFourOfAKind and 4 of a kind exists" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Ten, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Ten, Spades))

    assert(!module.isThreeOfAKind(cards))
    assert(module.isFourOfAKind(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(result.isDefined)
    assert(result.get.length == 4)
    assert(result.get.count(c => Seq(Card(Ten, Hearts), Card(Ten, Diamonds), Card(Ten, Clubs), Card(Ten, Spades)).contains(c)) == result.get.length)
  }

  it should "not yield full house when isFullHouse is false (with 2 pair present)" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Ten, Clubs), 
      Card(Two, Diamonds), 
      Card(Two, Hearts),
      Card(Five, Clubs))

    assert(module.isTwoPair(cards))
    assert(!module.isFullHouse(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }

  it should "not yield full house when isFullHouse is false (with 3 of a kind present)" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Ten, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Five, Clubs))

    assert(module.isThreeOfAKind(cards))
    assert(!module.isFullHouse(cards))
    val result: Option[Seq[Card]] = module.fourOfAKind(cards)
    assert(!result.isDefined)
  }
  
  it should "yield full house when isFullHouse (with 3 of a kind and 1 pair present)" in {
    val cards: Seq[Card] = Seq(
      Card(Ten, Hearts), 
      Card(Ten, Clubs), 
      Card(Ten, Diamonds), 
      Card(Two, Hearts),
      Card(Two, Clubs))

    assert(module.isThreeOfAKind(cards))
    assert(module.isOnePair(cards))
    assert(module.isFullHouse(cards))
    val result: Option[Seq[Card]] = module.fullHouse(cards)
    assert(result.isDefined)
    assert(result.get.length == 5)
    assert(result.get.count(c => cards.contains(c)) == result.get.length)
  }

  it should "not yield a straight from an empty hand" in {
    val cards: Seq[Card] = Nil 
    assert(!module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(!result.isDefined)
  }

  it should "not yield a straight from a single card hand" in {
    val cards: Seq[Card] = Seq(
      Card(Seven, Hearts)
    )
    assert(!module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(!result.isDefined)
  }

  it should "not yield a straight from a two card hand with 2 cards of same rank" in {
    val cards: Seq[Card] = Seq(
      Card(Seven, Hearts),
      Card(Seven, Clubs),
    )
    assert(!module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(!result.isDefined)
  }

  it should "not yield a straight from a two card hand with 2 non-consecutive cards" in {
    val cards: Seq[Card] = Seq(
      Card(Seven, Hearts),
      Card(Five, Clubs),
    )
    assert(!module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(!result.isDefined)
  }

  it should "yield a straight from a two card hand with 2 consecutive cards" in {
    val cards: Seq[Card] = Seq(
      Card(Seven, Hearts),
      Card(Six, Clubs),
    )
    assert(module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(result.isDefined)
    assert(result.get.sorted == cards.sorted)
    assert(module.commons.sequenced(result.get))
  }

  it should "not yield a straight from a three card hand with 2 consecutive cards and one pair" in {
    val cards: Seq[Card] = Seq(
      Card(Seven, Hearts),
      Card(Seven, Clubs),
      Card(Six, Hearts),
    )
    assert(module.isOnePair(cards))
    assert(!module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(!result.isDefined)
  }

  it should "yield a straight from three consecutive cards" in {
    val cards: Seq[Card] = Seq(
      Card(Seven, Hearts),
      Card(Five, Clubs),
      Card(Six, Hearts),
    )
    assert(module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(result.isDefined)
    assert(result.get.sorted == cards.sorted)
    assert(module.commons.sequenced(result.get))
  }

  it should "yield a straight from five consecutive cards" in {
    val cards: Seq[Card] = Seq(
      Card(Seven, Hearts),
      Card(Five, Clubs),
      Card(Six, Hearts),
      Card(Four, Spades),
      Card(Eight, Spades),
    )
    assert(module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(result.isDefined)
    assert(result.get.sorted == cards.sorted)
    assert(module.commons.sequenced(result.get))
  }

  it should "yield a straight on a 5 card royal flush" in {
    val cards: Seq[Card] = Seq(
      Card(Ace, Diamonds),
      Card(King, Diamonds),
      Card(Queen, Diamonds),
      Card(Jack, Diamonds),
      Card(Ten, Diamonds)
    )
    assert(module.isRoyalFlush(cards))
    assert(module.isStraight(cards))
    val result: Option[Seq[Card]] = module.straight(cards)
    assert(result.isDefined)
    assert(module.commons.sequenced(result.get))
  }


  it should "not yield a flush on an empty hand" in {
    val cards: Seq[Card] = Nil 
    assert(!module.isFlush(cards))
    val result: Option[Seq[Card]] = module.flush(cards)
    assert(!result.isDefined)
  }

  it should "not yield a flush on a single card hand" in {
    val cards: Seq[Card] = Seq(
      Card(King, Diamonds)
    )
    assert(!module.isFlush(cards))
    val result: Option[Seq[Card]] = module.flush(cards)
    assert(!result.isDefined)
  }

  it should "not yield a flush on one pair with different suits" in {
    val cards: Seq[Card] = Seq(
      Card(King, Diamonds),
      Card(King, Clubs)
    )
    assert(!module.isFlush(cards))
    val result: Option[Seq[Card]] = module.flush(cards)
    assert(!result.isDefined)
  }

  it should "yield a flush on 2 cards of the same suit" in {
    val cards: Seq[Card] = Seq(
      Card(King, Diamonds),
      Card(Three, Diamonds)
    )
    assert(module.isFlush(cards))
    val result: Option[Seq[Card]] = module.flush(cards)
    assert(result.isDefined)
    assert(module.commons.suited(result.get).count(c => c.suit == Diamonds) == cards.length)
  }

  it should "not yield a flush on 5 card hand with only 1 card of a different suit" in {
    val cards: Seq[Card] = Seq(
      Card(King, Diamonds),
      Card(Two, Diamonds),
      Card(Five, Diamonds),
      Card(Queen, Diamonds),
      Card(Three, Spades)
    )
    assert(!module.isFlush(cards))
    val result: Option[Seq[Card]] = module.flush(cards)
    assert(!result.isDefined)
  }

  it should "yield a flush on a 5 card royal flush" in {
    val cards: Seq[Card] = Seq(
      Card(Ace, Diamonds),
      Card(King, Diamonds),
      Card(Queen, Diamonds),
      Card(Jack, Diamonds),
      Card(Ten, Diamonds)
    )
    assert(module.isRoyalFlush(cards))
    assert(module.isFlush(cards))
    val result: Option[Seq[Card]] = module.flush(cards)
    assert(result.isDefined)
    assert(module.commons.suited(result.get).count(c => c.suit == Diamonds) == cards.length)
  }
  
  it should "not not yield a straight flush on an empty hand" in {
    val cards: Seq[Card] = Nil 
    assert(!module.isStraightFlush(cards))
    val result: Option[Seq[Card]] = module.straightFlush(cards)
    assert(!result.isDefined)
  }

  it should "not yield a straight flush on a single card hand" in {
    val cards: Seq[Card] = Seq(
      Card(King, Diamonds)
    )
    assert(!module.isStraightFlush(cards))
    val result: Option[Seq[Card]] = module.straightFlush(cards)
    assert(!result.isDefined)
  }

  it should "not yield a straight flush on a straight that's not a flush: all but one card sharing the same suit" in {
    val cards: Seq[Card] = Seq(
      Card(Five, Diamonds),
      Card(Six, Diamonds),
      Card(Seven, Clubs),
      Card(Eight, Diamonds),
      Card(Nine, Diamonds)
    )
    assert(module.isStraight(cards))
    assert(!module.isFlush(cards))
    val result: Option[Seq[Card]] = module.straightFlush(cards)
    assert(!result.isDefined)
  }

  it should "not yield a straight flush on a flush that's not a straight" in {
    val cards: Seq[Card] = Seq(
      Card(Ace, Spades),
      Card(Six, Spades),
      Card(Seven, Spades),
      Card(Eight, Spades),
      Card(Nine, Spades)
    )
    assert(!module.isStraight(cards))
    assert(module.isFlush(cards))
    val result: Option[Seq[Card]] = module.straightFlush(cards)
    assert(!result.isDefined)
  }

  it should "yield a straight flush on a straight that's also a flush" in {
    val cards: Seq[Card] = Seq(
      Card(Six, Spades),
      Card(Seven, Spades),
      Card(Eight, Spades),
      Card(Nine, Spades),
      Card(Ten, Spades),
    )
    assert(module.isStraight(cards))
    assert(module.isFlush(cards))
    val result: Option[Seq[Card]] = module.straightFlush(cards)
    assert(result.isDefined)
    assert(module.commons.suited(result.get).count(c => c.suit == Spades) == cards.length)
    assert(module.commons.sequenced(result.get))
  }

  it should "yield a straight flush on a royal flush" in {
    val cards: Seq[Card] = Seq(
      Card(Ace, Diamonds),
      Card(King, Diamonds),
      Card(Queen, Diamonds),
      Card(Jack, Diamonds),
      Card(Ten, Diamonds)
    )
    assert(module.isRoyalFlush(cards))
    assert(module.isStraightFlush(cards))
    val result: Option[Seq[Card]] = module.straightFlush(cards)
    assert(result.isDefined)
    assert(module.commons.sequenced(result.get))
    assert(module.commons.suited(result.get).count(c => c.suit == Diamonds) == cards.length)
  }

  it should "not yield a royal flush on an empty hand" in {
    val cards: Seq[Card] = Nil 
    assert(!module.isRoyalFlush(cards))
    val result: Option[Seq[Card]] = module.royalFlush(cards)
    assert(!result.isDefined)
  }

  it should "not yield a royal flush on a single card hand" in {
    val cards: Seq[Card] = Seq(
      Card(King, Diamonds)
    )
    assert(!module.isRoyalFlush(cards))
    val result: Option[Seq[Card]] = module.royalFlush(cards)
    assert(!result.isDefined)
  }

  it should "not yield a royal flush on a 4 card hand of highest 4 ranks all of same suit" in {
    val cards: Seq[Card] = Seq(
      Card(Ace, Diamonds),
      Card(King, Diamonds),
      Card(Queen, Diamonds),
      Card(Jack, Diamonds)
    )
    assert(!module.isRoyalFlush(cards))
    val result: Option[Seq[Card]] = module.royalFlush(cards)
    assert(!result.isDefined)
  }

  it should "yield a royal flush on a 5 card hand of highest 5 ranks all of same suit" in {
    val cards: Seq[Card] = Seq(
      Card(Ace, Diamonds),
      Card(King, Diamonds),
      Card(Queen, Diamonds),
      Card(Jack, Diamonds),
      Card(Ten, Diamonds)
    )
    assert(module.isRoyalFlush(cards))
    val result: Option[Seq[Card]] = module.royalFlush(cards)
    assert(result.isDefined)
    assert(module.commons.sequenced(result.get))
    assert(module.commons.suited(result.get).count(c => c.suit == Diamonds) == cards.length)
  }

  it should "not yield a hand type on an empty hand" in {
    val cards: Seq[Card] = Nil
    val result: Option[PokerHandType] = module.handType(cards)
    assert(result.isEmpty)
    assert(!result.isDefined)
  }

  it should "yield HighCard hand type on a single card hand" in {
    val cards: Seq[Card] = Seq(Card(Queen, Hearts))
    val result: Option[PokerHandType] = module.handType(cards)
    assert(!result.isEmpty)
    assert(result.isDefined)
    assert(result.get == HighCard)
  }

  it should "yield HighCard hand type on a 2 card hand with no matching ranks or suits" in {
    val cards: Seq[Card] = Seq(Card(Queen, Hearts), Card(Three, Spades))
    val result: Option[PokerHandType] = module.handType(cards)
    assert(!result.isEmpty)
    assert(result.isDefined)
    assert(result.get == HighCard)
  }

  it should "yield Flush hand type on a 2 card hand with different ranks but matching suits" in {
    val cards: Seq[Card] = Seq(Card(Queen, Spades), Card(Three, Spades))
    val result: Option[PokerHandType] = module.handType(cards)
    assert(!result.isEmpty)
    assert(result.isDefined)
    assert(result.get == Flush)
  }

  it should "yield Straight hand type on 3 consecutive cards of different suits" in {
    val cards: Seq[Card] = Seq(Card(Queen, Spades), Card(King, Hearts), Card(Ace, Spades))
    val result: Option[PokerHandType] = module.handType(cards)
    assert(!result.isEmpty)
    assert(result.isDefined)
    assert(result.get == Straight)
  }

  it should "yield StraightFlush hand type on 5 consecutive cards all belonging to the same suit, and the cards are not the highest" in {
    val cards: Seq[Card] = Seq(Card(Seven, Spades), Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades), Card(Jack, Spades))
    val result: Option[PokerHandType] = module.handType(cards)
    assert(!result.isEmpty)
    assert(result.isDefined)
    assert(result.get == StraightFlush)
  }

  it should "yield RoyalStraightFlush hand type on 5 consecutive cards all belonging to the same suit, and the cards are highest" in {
    val cards: Seq[Card] = Seq(Card(Ten, Spades), Card(Jack, Spades), Card(Queen, Spades), Card(King, Spades), Card(Ace, Spades))
    val result: Option[PokerHandType] = module.handType(cards)
    assert(!result.isEmpty)
    assert(result.isDefined)
    assert(result.get == RoyalFlush)
  }

  it should "yield ThreeOfAKind hand type on 5 cards in which 3 of the cards match in rank" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Spades), Card(Jack, Hearts), Card(King, Spades), Card(Two, Hearts))
    val result: Option[PokerHandType] = module.handType(cards)
    assert(!result.isEmpty)
    assert(result.isDefined)
    assert(result.get == ThreeOfAKind)

  }
}