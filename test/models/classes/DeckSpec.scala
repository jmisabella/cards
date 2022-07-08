package cards.models.classes

import cards.models.classes.{ Card, Deck }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class DeckSpec extends AnyFlatSpec {
  "Deck" should "have Deck(AllCards) yield 54 cards, including 2 jokers" in {
    val deck: Deck = Deck(AllCards)
    assert(deck.length == 54)
    assert(deck.count(_.isJoker) == 2)
  }

  it should "have Deck() yield 54 cards, including 2 jokers" in {
    val deck: Deck = Deck()
    assert(deck.length == 54)
    assert(deck.count(_.isJoker) == 2)
  }

  it should "exclude 1 left-bower joker from the deck, yielding 53 cards" in {
    val deck: Deck = Deck(excluded = Seq(Card(LeftBower, Joker)))
    assert(deck.length == 53)
    assert(deck.count(_.isJoker) == 1)
    assert(!deck.contains(Card(LeftBower, Joker)))
  }

  it should "exclude both jokers from the deck, yielding 52 cards" in {
    val deck: Deck = Deck(excluded = Seq(Card(LeftBower, Joker), Card(RightBower, Joker)))
    assert(deck.length == 52)
    assert(deck.count(_.isJoker) == 0)
    assert(!deck.contains(Card(LeftBower,Joker)))
    assert(!deck.contains(Card(RightBower, Joker)))
  }

  it should "create a 3 deck shoe deck excluding jokers, which has 52 * 3 cards total" in {
    val deck: Deck = Deck(excluded = Seq(Card(LeftBower, Joker), Card(RightBower, Joker)), numberOfDecks = 3)
    assert(deck.length == 52 * 3)
    assert(deck.count(_.isJoker) == 0)
  }

  it should "have Euchre deck not contain any jokers nor any cards below 9" in {
    val deck: Deck = Deck(Euchre)
    assert(deck.length == 24)
    assert(!deck.contains(Seq(Card(LeftBower, Joker), Card(RightBower, Joker)) ++
      (for { r <- Seq(Two, Three, Four, Five, Six, Seven, Eight); s <- Suit.values} yield Card(r, s)).toList))
    assert(deck.contains( (for { r <- Seq(Nine, Ten, Jack, Queen, King, Ace); s <- Suit.values} yield Card(r, s)).toList))
  }

  it should "deal 0 cards" in {
    val deck: Deck = Deck()
    assert(deck.length == 54)
    val (dealt, next): (Seq[Card], Deck) = deck.deal(0)
    assert(dealt == Nil)    
    assert(next.length == 54)    
  }

  it should "deal 1 card" in {
    val deck: Deck = Deck()
    assert(deck.length == 54)
    val (dealt, next): (Seq[Card], Deck) = deck.deal()
    assert(dealt.length == 1)
    assert(next.length == 53)
    assert(!next.contains(dealt.head))
  }

  it should "deal 2 cards" in {
    val deck: Deck = Deck()
    assert(deck.length == 54)
    val (dealt, next): (Seq[Card], Deck) = deck.deal(2)
    assert(dealt.length == 2)
    assert(next.length == 52)
    assert(!next.contains(dealt.head))
    assert(!next.contains(dealt(1)))

    println("CARD 1: " + dealt.head)
    println("CARD 2: " + dealt(1))
  }

  it should "deal all cards in the deck" in {
    val deck: Deck = Deck()
    assert(deck.length == 54)
    val (dealt, next): (Seq[Card], Deck) = deck.deal(54)
    assert(dealt.length == 54)
    assert(next.length == 0)
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


}