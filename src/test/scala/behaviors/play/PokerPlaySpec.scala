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
import cards.behaviors.predicates.PokerPredicates
import cards.behaviors.evaluation.PokerHandEvaluation
import cards.behaviors.Commons

class PokerPlaySpec extends AnyFlatSpec with GivenWhenThen {

  private case object _commons extends Commons
  private case object _predicates extends PokerPredicates {
    override type CB = Commons
    override val commons = _commons
  }
  private case object _evaluation extends PokerHandEvaluation {
    override type P = PokerPredicates
    override val predicates = _predicates
  }
  private case object module extends PokerPlay {
    override type EVAL = PokerHandEvaluation
    override val evaluation = _evaluation
  }
  import module._

  "PokerPlay" should "know which cards belong to which players" in {
    Given("hands belonging to Brandon and Brittany including 2 cards shared between them") 
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

  it should "select the best 5-card hand out of 7 cards" in {
    Given("""hands belonging to Brandon and Brittany from a total of 7 cards, 3 of which are shared between the players""")
    val hands: Seq[Hand] = Seq(
      Hand(owners = Seq("Brandon", "Brittany"), hand = Seq(Card(Two, Spades), Card(Ten, Clubs), Card(Ace, Clubs))),
      Hand(owners = Seq("Brandon"), hand = Seq(Card(Two, Clubs), Card(Two, Hearts), Card(Two, Diamonds), Card(Ace, Spades))),
      Hand(owners = Seq("Brittany"), hand = Seq(Card(King, Clubs), Card(Queen, Hearts), Card(Jack, Hearts), Card(Ten, Diamonds)))
    )
    When("determining Brandon's best 5-card hand from his 7 total cards")
    var result: Seq[Card] = cards(hands, "Brandon").sorted
    Then("Brandon's selected cards would be 4-of-a-kind (Twos) with the 5th card being the Ace of Clubs")
    evaluation.predicates.isFourOfAKind(result) shouldBe true 
    result should equal (Seq(Card(Two, Clubs), Card(Two, Diamonds), Card(Two, Hearts), Card(Two, Spades), Card(Ace, Clubs)))

    When("determining Brittany's best 5-card hand from her 7 total cards")
    result = cards(hands, "Brittany").sorted
    Then("Brittany's selected ards would be a straight: Ten through Ace but consisting of different Suits")
    // result.map(_.rank) should equal (Seq(Card(Ten, Diamonds), Card(Jack, Hearts), Card(Queen, Hearts), Card(King, Clubs), Card(Ace, Clubs)).sorted)
    evaluation.predicates.isStraight(result) shouldBe true 
    evaluation.predicates.isStraightFlush(result) shouldBe false
    result.map(_.rank) should equal (Seq(Ten, Jack, Queen, King, Ace).sorted)
  }
}