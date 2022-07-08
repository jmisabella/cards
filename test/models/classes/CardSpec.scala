package cards.models.classes

import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class CardSpec extends AnyFlatSpec {

  "Card" should "consider an empty list as sorted" in {
    val cards: List[Card] = Nil
    assert(cards.sorted == cards)
  }

  it should "consider single card list as sorted" in {
    val cards: List[Card] = List(Card(Three, Hearts)) 
    assert(cards.sorted == cards)
  }

  it should "consider 2 identical cards with same rank but different suit as sorted" in {
    val cards: List[Card] = List(Card(Seven, Diamonds), Card(Seven, Diamonds)) 
    assert(cards.sorted == cards)
  }

  it should "consider 2 cards with same rank but different suit as sorted" in {
    val cards: List[Card] = List(Card(Seven, Diamonds), Card(Seven, Hearts)) 
    assert(cards.sorted == cards)
  }
  
  it should "consider 2 sorted differently ranked cards as sorted" in {
    val cards: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades)) 
    assert(cards.sorted == cards)
  }

  it should "consider 2 unsorted differently ranked cards as unsorted" in {
    val cards: List[Card] = List(Card(Queen, Diamonds), Card(Jack, Spades)) 
    assert(cards.sorted != cards)
  }

  it should "consider 3 unsorted multi ranked cards as unsorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(Card(Queen, Clubs), Card(Queen, Diamonds), Card(Eight, Spades)) 
    // println("SORTED: " + cards.sorted)
    assert(cards.sorted != cards && cards != List(Card(Queen, Diamonds), Card(Queen, Clubs), Card(Eight, Spades)))
  }
  
  it should "1: consider 3 sorted multi ranked cards as sorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(Card(Queen, Clubs), Card(Queen, Diamonds), Card(King, Spades)) 
    // println("SORTED: " + cards.sorted)
    assert(cards.sorted == cards || cards.sorted == List(Card(Queen, Diamonds), Card(Queen, Clubs), Card(King, Spades)))
  }
  
  it should "2: consider 3 sorted multi ranked cards as sorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(Card(Three, Clubs), Card(Three, Diamonds), Card(Nine, Spades)) 
    // println("SORTED: " + cards.sorted)
    assert(cards.sorted == cards || cards.sorted == List(Card(Three, Diamonds), Card(Three, Clubs), Card(Nine, Spades)))
  }
}