package cards.models.behaviors.nonplayer

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.behaviors.nonplayer.BlackjackNonPlayer
import cards.models.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackNonPlayerSpec extends AnyFlatSpec with GivenWhenThen {
  private [nonplayer] case object _commons extends Commons
  private [nonplayer] case object _evaluation extends BlackjackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  private [nonplayer] case object _predicates extends BlackjackPredicates {
    override type CB = Commons
    override val commons = _commons
  }
  case object module extends BlackjackNonPlayer {
    override type EVAL = BlackjackHandEvaluation
    override type PREDICATES = BlackjackPredicates
    override val evaluation = _evaluation
    override val predicates = _predicates
  }

  "BlackjackNonPlayer" should "throw an illegal state exception when proceeding to next state from a game state without any players" in {
    Given("a blackjack game state without any players")
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Nil, players = Nil)
    When("proceeding to the next state")
    Then("an illegal state exception should be thrown")
    an [IllegalStateException] shouldBe thrownBy (module.next(gameState)) 
  }

  it should "throw an illegal state exception when proceeding to next state from a game state whose deck is empty" in {
    Given("a blackjack game state whose deck has no remaining cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Four, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        wins = None)))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(Seq(Card(Two, Clubs), Card(Ace, Spades)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        wins = None)))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Three, Spades), Card(Seven, Hearts)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        wins = None)))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), deck = Deck.emptyDeck, dealerHand = dealerCards, players = Seq(player1, player2, player3))
    When("proceeding to the next state")
    Then("an illegal state exception should be thrown")
    an [IllegalStateException] shouldBe thrownBy (module.next(gameState)) 
  }
  
  it should "stand on a 3 card hand whose value is 20" in {
    
    pending
  }
}