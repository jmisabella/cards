package cards.models.classes

import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CardSpec extends AnyFlatSpec {

  "Card" should "consider an empty list as sorted" in {
    val cards: List[Card] = Nil
    cards shouldBe sorted
  }

  it should "consider single card list as sorted" in {
    val cards: List[Card] = List(Card(Three, Hearts)) 
    cards shouldBe sorted
  }

  it should "consider 2 identical cards with same rank but different suit as sorted" in {
    val cards: List[Card] = List(Card(Seven, Diamonds), Card(Seven, Diamonds)) 
    cards.sorted should equal (cards) 
  }

  it should "consider 2 cards with same rank but different suit as sorted" in {
    val cards: List[Card] = List(Card(Seven, Diamonds), Card(Seven, Hearts)) 
    cards.sorted should equal (cards) 
  }
  
  it should "consider 2 sorted differently ranked cards as sorted" in {
    val cards: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades)) 
    cards.sorted should equal (cards) 
  }

  it should "consider 2 unsorted differently ranked cards as unsorted" in {
    val cards: List[Card] = List(Card(Queen, Diamonds), Card(Jack, Spades)) 
    cards.sorted should not equal (cards) 
  }

  it should "consider 3 unsorted multi ranked cards as unsorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(Card(Queen, Clubs), Card(Queen, Diamonds), Card(Eight, Spades)) 
    cards.sorted should not equal (cards) 
    cards should not equal (List(Card(Queen, Diamonds), Card(Queen, Clubs), Card(Eight, Spades)))
  }
  
  it should "1: consider 3 sorted multi ranked cards as sorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(Card(Queen, Clubs), Card(Queen, Diamonds), Card(King, Spades)) 
    cards.sorted should (
      equal (cards) or
      equal (List(Card(Queen, Diamonds), Card(Queen, Clubs), Card(King, Spades)))
    )
  }
  
  it should "2: consider 3 sorted multi ranked cards as sorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(Card(Three, Clubs), Card(Three, Diamonds), Card(Nine, Spades)) 
    cards.sorted should (
      equal (cards) or
      equal (List(Card(Three, Diamonds), Card(Three, Clubs), Card(Nine, Spades)))
    )
  }
}