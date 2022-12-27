package behaviors.play

import cards.behaviors.play.PokerPlay
import cards.classes.{ Card, Rank, Suit, Deck, DeckType, Outcome }
import cards.classes.DeckType._
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.hand.Hand
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class PokerPlaySpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends PokerPlay
  import module._

  "PokerPlay" should "know which cards belong to which players" in {
    Given("3 hands, two belonging to Brandon and Brittany and one shared between Brandon and Brittany") 
    val hands: Seq[Hand] = Seq(
      Hand(hand = Seq(Card(Four, Clubs), Card(Six, Hearts)), owners = Seq("Brandon", "Brittany")), 
      Hand(hand = Seq(Card(Two, Hearts), Card(Three, Clubs), Card(Five, Diamonds)), owners = Seq("Brandon")), 
      Hand(hand = Seq(Card(Six, Clubs), Card(Six, Diamonds), Card(King, Diamonds)), owners = Seq("Brittany")))
  
    When("determining which cards belong to Jeffrey")
    var result: Seq[Card] = cards(hands, "Jeffrey")
    Then("an empty list should be yielded")
    result shouldBe empty

    When("determining which cards belong to Brandon")
    result = cards(hands, "Brandon").sorted
    Then("Two of Hearts, Three of Clubs, Four of Clubs, Five of Diamonds, and Six of Hearts should be yielded")
    result should equal (Seq(Card(Two, Hearts), Card(Three, Clubs), Card(Four, Clubs), Card(Five, Diamonds), Card(Six, Hearts)))
  
    When("determining which cards belong to Brittany")
    result = cards(hands, "Brittany").sorted
    Then("Four of Clubs, Six of Clubs, Six of Diamonds, Six of Hearts, and King of Diamonds should be yielded")
    result should equal (Seq(Card(Four, Clubs), Card(Six, Clubs), Card(Six, Diamonds), Card(Six, Hearts), Card(King, Diamonds)).sorted)
  
  }
}