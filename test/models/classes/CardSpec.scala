package cards.models.classes

import cards.models.classes.{ SuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class CardSpec extends AnyFlatSpec {

  "Card" should "consider an empty list as sorted" in {
    val cards: List[Card] = Nil
    assert(cards.sorted == cards)
  }

  it should "consider single card list as sorted" in {
    val cards: List[Card] = List(SuitedCard(Three, Hearts)) 
    assert(cards.sorted == cards)
  }

  it should "consider 2 identical cards with same rank but different suit as sorted" in {
    val cards: List[Card] = List(SuitedCard(Seven, Diamonds), SuitedCard(Seven, Diamonds)) 
    assert(cards.sorted == cards)
  }

  it should "consider 2 cards with same rank but different suit as sorted" in {
    val cards: List[Card] = List(SuitedCard(Seven, Diamonds), SuitedCard(Seven, Hearts)) 
    assert(cards.sorted == cards)
  }
  
  it should "consider 2 sorted differently ranked cards as sorted" in {
    val cards: List[Card] = List(SuitedCard(Four, Hearts), SuitedCard(Nine, Spades)) 
    assert(cards.sorted == cards)
  }

  it should "consider 2 unsorted differently ranked cards as unsorted" in {
    val cards: List[Card] = List(SuitedCard(Queen, Diamonds), SuitedCard(Jack, Spades)) 
    assert(cards.sorted != cards)
  }

  it should "consider 3 unsorted multi ranked cards as unsorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(SuitedCard(Queen, Clubs), SuitedCard(Queen, Diamonds), SuitedCard(Eight, Spades)) 
    // println("SORTED: " + cards.sorted)
    assert(cards.sorted != cards && cards != List(SuitedCard(Queen, Diamonds), SuitedCard(Queen, Clubs), SuitedCard(Eight, Spades)))
  }
  
  it should "1: consider 3 sorted multi ranked cards as sorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(SuitedCard(Queen, Clubs), SuitedCard(Queen, Diamonds), SuitedCard(King, Spades)) 
    // println("SORTED: " + cards.sorted)
    assert(cards.sorted == cards || cards.sorted == List(SuitedCard(Queen, Diamonds), SuitedCard(Queen, Clubs), SuitedCard(King, Spades)))
  }
  
  it should "2: consider 3 sorted multi ranked cards as sorted, when 2 cards are of the same rank" in {
    val cards: List[Card] = List(SuitedCard(Three, Clubs), SuitedCard(Three, Diamonds), SuitedCard(Nine, Spades)) 
    // println("SORTED: " + cards.sorted)
    assert(cards.sorted == cards || cards.sorted == List(SuitedCard(Three, Diamonds), SuitedCard(Three, Clubs), SuitedCard(Nine, Spades)))
  }
}