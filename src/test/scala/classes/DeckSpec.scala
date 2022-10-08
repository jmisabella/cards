package cards.classes

import cards.classes.{ Card, Deck }
import cards.classes.DeckType._
import cards.classes.Rank._
import cards.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class DeckSpec extends AnyFlatSpec {
  "Deck" should "have Deck(AllCards) yield 54 cards, including 2 jokers" in {
    val deck: Deck = Deck(AllCards)
    deck should have length (54)
    deck.count(_.isJoker) should be (2)
  }

  it should "have Deck() yield 54 cards, including 2 jokers" in {
    val deck: Deck = Deck()
    deck should have length (54)
    deck.count(_.isJoker) should be (2)
  }

  it should "exclude 1 left-bower joker from the deck, yielding 53 cards" in {
    val deck: Deck = Deck(excluded = Seq(Card(LeftBower, Joker)))
    deck should have length (53)
    deck.count(_.isJoker) should be (1)
    deck.cards should not contain (Card(LeftBower, Joker)) 
  }

  it should "exclude both jokers from the deck, yielding 52 cards" in {
    val deck: Deck = Deck(excluded = Seq(Card(LeftBower, Joker), Card(RightBower, Joker)))
    deck should have length (52)
    deck.count(_.isJoker) should be (0)
    deck.cards should not contain (Card(LeftBower, Joker)) 
    deck.cards should not contain (Card(RightBower, Joker)) 
  }

  it should "create a 3 deck shoe deck excluding jokers, which has 52 * 3 cards total" in {
    val deck: Deck = Deck(excluded = Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), numberOfDecks = 3)
    deck should have length (52 * 3) 
    deck.count(_.isJoker) should be (0)
  }

  it should "have Euchre deck not contain any jokers nor any cards below 9" in {
    val deck: Deck = Deck(Euchre)
    deck should have length (24)
    assert(!deck.contains(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)) ++
      (for { r <- Seq(Two, Three, Four, Five, Six, Seven, Eight); s <- Suit.values} yield Card(r, s)).toList))
    assert(deck.contains( (for { r <- Seq(Nine, Ten, Jack, Queen, King, Ace); s <- Suit.values} yield Card(r, s)).toList))
  }

  it should "deal 0 cards" in {
    val deck: Deck = Deck()
    deck should have length (54)    
    val (dealt, next): (Seq[Card], Deck) = deck.deal(0)
    dealt shouldBe empty 
    next should have length (54)    
  }

  it should "deal 1 card" in {
    val deck: Deck = Deck()
    deck should have length (54)
    val (dealt, next): (Seq[Card], Deck) = deck.deal()
    dealt should have length (1)
    next should have length (53)
    next.cards should not contain (dealt.head) 
  }

  it should "deal 2 cards" in {
    val deck: Deck = Deck()
    deck should have length (54)
    val (dealt, next): (Seq[Card], Deck) = deck.deal(2)
    dealt should have length (2)
    next should have length (52)
    next.cards should not contain (dealt.head)
    next.cards should not contain (dealt(1))
  }

  it should "deal all cards in the deck" in {
    val deck: Deck = Deck()
    deck should have length 54
    val (dealt, next): (Seq[Card], Deck) = deck.deal(54)
    dealt should have length (54)
    next should have length (0)
  }

  it should "error when unable to deal more cards than available in the deck" in {
    val deck: Deck = Deck()
    assert(deck.length == 54)
    try {
      val (dealt, next): (Seq[Card], Deck) = deck.deal(55)
      assert(false)
    } catch {
      case e: IllegalArgumentException => assert(true)
      case _: Exception => assert(false)
    }
  }

  "Deck.emptyDeck" should "contain no cards" in {
    Deck.emptyDeck.cards shouldBe empty
  }

}