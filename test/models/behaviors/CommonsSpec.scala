package cards.models.behaviors

import cards.models.behaviors.Commons
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class CommonsSpec extends AnyFlatSpec {

  case object module extends Commons

  "Commons" should "count ranks and suits from am empty list as both being zero" in {
    val cards: Seq[Card] = Nil
    val ranks: Map[Rank, Int] = module.countRank(cards)
    val suits: Map[Suit, Int] = module.countSuit(cards)

    assert(ranks(Four) == 0)
    assert(ranks(Ace) == 0)
    assert(ranks(King) == 0)
    assert(suits(Hearts) == 0)
    assert(suits(Diamonds) == 0)
    assert(suits(Spades) == 0)
    assert(suits(Clubs) == 0)
  }
  it should "count when 3 cards are different except that 2 share a common rank" in {
    val cards: Seq[Card] = Seq(Card(Four, Hearts), Card(Four, Diamonds), Card(Ace, Spades))
    val ranks: Map[Rank, Int] = module.countRank(cards)
    val suits: Map[Suit, Int] = module.countSuit(cards)
    assert(ranks(Four) == 2)
    assert(ranks(Ace) == 1)
    assert(ranks(King) == 0)
    assert(suits(Hearts) == 1)
    assert(suits(Diamonds) == 1)
    assert(suits(Spades) == 1)
    assert(suits(Clubs) == 0)
  }

  it should "get max rank from empty list as None" in {
    val cards: Seq[Card] = Nil
    val max: Option[Rank] = module.maxRank(cards)
    assert(max.isEmpty)
  }
  it should "get min rank from empty list as None" in {
    val cards: Seq[Card] = Nil
    val max: Option[Rank] = module.minRank(cards)
    assert(max.isEmpty)
  }

  it should "get high card(s) from empty list as empty list" in {
    val cards: Seq[Card] = Nil
    val high: Seq[Card] = module.highest(cards)
    assert(high.isEmpty)
  }
  
  it should "get low card(s) from empty list as empty list" in {
    val cards: Seq[Card] = Nil
    val high: Seq[Card] = module.lowest(cards)
    assert(high.isEmpty)
  }
  
  it should "get max rank from single item list as that card's rank" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val max: Option[Rank] = module.maxRank(cards)
    assert(max.isDefined)
    assert(max.get == Eight)
  }
  
  it should "get min rank from single item list as that card's rank" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val max: Option[Rank] = module.minRank(cards)
    assert(max.isDefined)
    assert(max.get == Eight)
  }

  it should "get high card(s) from single card list as that card" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val high: Seq[Card] = module.highest(cards)
    assert(high.length == 1)
    assert(high == cards)
  }

  it should "get low card(s) from single card list as that card" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val low: Seq[Card] = module.lowest(cards)
    assert(low.length == 1)
    assert(low == cards)
  }

  it should "get high card(s) from two card list with differing ranks and suits as the one high card" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val high: Seq[Card] = module.highest(cards)
    assert(high.length == 1)
    assert(high.head == Card(Nine, Diamonds))
  }

  it should "get low card(s) from two card list with differing ranks and suits as the one low card" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val high: Seq[Card] = module.lowest(cards)
    assert(high.length == 1)
    assert(high.head == Card(Four, Hearts))
  }

  it should "get high card(s) from two card list with differing ranks and suits but filtered by lower card's suit" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val high: Seq[Card] = module.highest(cards, Some(Hearts))
    assert(high.length == 1)
    assert(high.head == Card(Four, Hearts))
  }
  
  it should "get low card(s) from two card list with differing ranks and suits but filtered by higher card's suit" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val high: Seq[Card] = module.lowest(cards, Some(Diamonds))
    assert(high.length == 1)
    assert(high.head == Card(Nine, Diamonds))
  }


  it should "not consider an empty list to be sequenced" in {
    val cards: Seq[Card] = Nil
    val result: Boolean = module.sequenced(cards)
    assert(!result)
  }

  it should "not consider a single card list to be sequenced" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs)) 
    val result: Boolean = module.sequenced(cards)
    assert(!result)
  }

  it should "not consider unsequenced list of 2 cards to be sequenced" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Nine, Diamonds)) 
    val result: Boolean = module.sequenced(cards)
    assert(!result)
  }

  it should "consider sequenced list of 2 cards to be sequenced" in {
    val cards: Seq[Card] = Seq(Card(Ten, Hearts), Card(Nine, Diamonds)) 
    val result: Boolean = module.sequenced(cards)
    assert(result)
  }
}