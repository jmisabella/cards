package cards.behaviors

import cards.behaviors.Commons
import cards.classes.{ Card, Rank, Suit }
import cards.classes.Rank._
import cards.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class CommonsSpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends Commons

  "Commons" should "count ranks and suits from am empty list as both being zero" in {
    val cards: Seq[Card] = Nil
    val ranks: Map[Rank, Int] = module.countRank(cards)
    val suits: Map[Suit, Int] = module.countSuit(cards)
    ranks(Four) should equal (0)
    ranks(Ace) should equal (0)
    ranks(King) should equal (0)
    suits(Hearts) should equal (0)
    suits(Diamonds) should equal (0)
    suits(Spades) should equal (0)
    suits(Clubs) should equal (0)
  }
  it should "count when 3 cards are different except that 2 share a common rank" in {
    val cards: Seq[Card] = Seq(Card(Four, Hearts), Card(Four, Diamonds), Card(Ace, Spades))
    val ranks: Map[Rank, Int] = module.countRank(cards)
    val suits: Map[Suit, Int] = module.countSuit(cards)
    ranks(Four) should equal (2)
    ranks(Ace) should equal (1)
    ranks(King) should equal (0)
    suits(Hearts) should equal (1)
    suits(Diamonds) should equal (1)
    suits(Spades) should equal (1)
    suits(Clubs) should equal (0)
  }

  it should "get max rank from empty list as None" in {
    Given ("an empty list") 
    val cards: Seq[Card] = Nil

    When ("calling for maxRank") 
    val max: Option[Rank] = module.maxRank(cards)
    
    Then ("result should be empty") 
    max shouldBe empty
  }
  it should "get min rank from empty list as None" in {
    Given ("an empty list") 
    val cards: Seq[Card] = Nil
  
    When ("calling for minRank") 
    val min: Option[Rank] = module.minRank(cards)
 
    Then ("result should be empty") 
    min shouldBe empty
  }

  it should "get high card(s) from empty list as empty list" in {
    val cards: Seq[Card] = Nil
    val high: Seq[Card] = module.highest(cards)
    high shouldBe empty
  }
  
  it should "get low card(s) from empty list as empty list" in {
    val cards: Seq[Card] = Nil
    val low: Seq[Card] = module.lowest(cards)
    low shouldBe empty 
  }
  
  it should "get max rank from single item list as that card's rank" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val max: Option[Rank] = module.maxRank(cards)
    max shouldBe defined
    max should contain (Eight)
  }
  
  it should "get min rank from single item list as that card's rank" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val min: Option[Rank] = module.minRank(cards)
    min shouldBe defined
    min should contain (Eight)
  }

  it should "get high card(s) from single card list as that card" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val high: Seq[Card] = module.highest(cards)
    high should have length (1)
    high should equal (cards) 
  }

  it should "get low card(s) from single card list as that card" in {
    val cards: Seq[Card] = Seq(Card(Eight, Clubs))
    val low: Seq[Card] = module.lowest(cards)
    low should have length (1)
    low should equal (cards) 
  }

  it should "get high card(s) from two card list with differing ranks and suits as the one high card" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val high: Seq[Card] = module.highest(cards)
    high should have length (1)
    high.head should equal (Card(Nine, Diamonds))
  }

  it should "get low card(s) from two card list with differing ranks and suits as the one low card" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val low: Seq[Card] = module.lowest(cards)
    low should have length (1)
    low.head should equal (Card(Four, Hearts))
  }

  it should "get high card(s) from two card list with differing ranks and suits but filtered by lower card's suit" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val high: Seq[Card] = module.highest(cards, Some(Hearts))
    high should have length (1)
    high.head should equal (Card(Four, Hearts))
  }
  
  it should "get low card(s) from two card list with differing ranks and suits but filtered by higher card's suit" in {
    val cards: Seq[Card] = Seq(Card(Nine, Diamonds), Card(Four, Hearts))
    val high: Seq[Card] = module.lowest(cards, Some(Diamonds))
    high should have length (1)
    high.head should equal (Card(Nine, Diamonds)) 
  }


  it should "not consider an empty list to be sequenced" in {
    val cards: Seq[Card] = Nil
    val result: Boolean = module.sequenced(cards)
    result should be (false)
  }

  it should "not consider a single card list to be sequenced" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs)) 
    val result: Boolean = module.sequenced(cards)
    result should be (false)
  }

  it should "not consider unsequenced list of 2 cards to be sequenced" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Nine, Diamonds)) 
    val result: Boolean = module.sequenced(cards)
    result should be (false)
  }

  it should "consider sequenced list of 2 cards to be sequenced" in {
    val cards: Seq[Card] = Seq(Card(Ten, Hearts), Card(Nine, Diamonds)) 
    val result: Boolean = module.sequenced(cards)
    result should be (true)
  }
}