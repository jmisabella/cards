package cards.models.classes.state

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackStateSpec extends AnyFlatSpec with GivenWhenThen {

  "BlackjackState" should "throw unexpected state exception when calling current player when no players exist" in {
    Given("a blackjack game state with no players")
    val state: BlackjackGameState = BlackjackGameState(players = Nil, dealerHand = Hand())
    When("retrieving the current player")
    Then("an illegal state exception should be thrown")
    an [IllegalStateException] shouldBe thrownBy (state.currentPlayer())
  }

  it should "throw unexpected state exception when calling current hand when no players have any hands" in {
    Given("a blackjack game state with 2 players, Patrick and Avery, who don't have any hands")
    val player1 = BlackjackPlayerState("Patrick Stewart", 20, Nil)
    val player2 = BlackjackPlayerState("Avery Brooks", 20, Nil)
    val state: BlackjackGameState = BlackjackGameState(players = Seq(player1, player2), dealerHand = Hand(), currentPlayerIndex = Some(0))
    When("retrieving the current player")
    val currentPlayer = state.currentPlayer()
    Then("Patrick should be retrieved") 
    currentPlayer should equal (player1)
    When("retrieving current player's current hand")
    Then("an illegal state exception should be thrown, since no players have any hands yet")
    an [IllegalStateException] shouldBe thrownBy (state.currentHand())
  }

  it should "retrieve current player's only hand" in {
    Given("a blackjack game state with 2 players, Patrick and Avery, each who have a single hand, and Avery's hand being specified as the current hand")
    val player1 = BlackjackPlayerState("Patrick Stewart", 20, Seq(Hand(Seq(Card(Seven, Hearts), Card(Ten, Diamonds)))))
    val player2 = BlackjackPlayerState("Avery Brooks", 20, Seq(Hand(Seq(Card(Four, Spades), Card(Seven, Clubs)))))
    val dealerHand = Hand(Seq(Card(Three, Hearts), Card(Eight, Clubs)))
    val state: BlackjackGameState = 
      BlackjackGameState(
        players = Seq(player1, player2), 
        dealerHand = dealerHand, 
        currentPlayerIndex = Some(1),
        currentHandIndex = Some(0))
    When("retrieving the current player")
    val currentPlayer = state.currentPlayer()
    Then("Avery should be retrieved")
    currentPlayer should equal (player2)
    When("retrieving current hand")
    val currentHand = state.currentHand()
    Then("Avery's cards should be retrieved")
    currentHand should equal (player2.hands.head)
  }

  it should "retrieve next hand index for a player who has 2 hands, from 1st hand to 2nd hand" in {
    Given("a blackjack game state with 1 player who has 2 hands, with first hand being current hand")
    val player = 
      BlackjackPlayerState(
        "Avery", 
        20, 
        Seq(Hand(Seq(Card(Seven, Hearts), Card(Ten, Diamonds))), Hand(Seq(Card(Three, Clubs), Card(Nine, Clubs)))))
    val dealerHand = Hand(Seq(Card(Three, Hearts), Card(Eight, Clubs)))
    val state: BlackjackGameState = 
      BlackjackGameState(
        players = Seq(player), 
        dealerHand = dealerHand, 
        currentPlayerIndex = Some(0),
        currentHandIndex = Some(0))
    When("retrieving next hand index")
    val nextHandIndex: Int = state.nextHandIndex()
    Then("1 should be retrieved as the next hand index")
    nextHandIndex should equal (1)
  }

  it should "consider the hand to be the last hand for a player who has 2 hands and the 2nd hand being the current hand" in {
    Given("a blackjack game state with 1 player who has 2 hands, with 2nd hand being current hand")
    val player = 
      BlackjackPlayerState(
        "Avery", 
        20, 
        Seq(Hand(Seq(Card(Seven, Hearts), Card(Ten, Diamonds))), Hand(Seq(Card(Three, Clubs), Card(Nine, Clubs)))))
    val dealerHand = Hand(Seq(Card(Three, Hearts), Card(Eight, Clubs)))
    val state: BlackjackGameState = 
      BlackjackGameState(
        players = Seq(player), 
        dealerHand = dealerHand, 
        currentPlayerIndex = Some(0),
        currentHandIndex = Some(1))
    When("checking whether current hand is player's last hand")
    val isLastHand: Boolean = state.isLastHand()
    Then("isLastHand should be true")
    isLastHand shouldBe (true)
  }

  it should "not consider the hand to be the last hand for a player who has 2 hands and the 1st hand being the current hand" in {
    Given("a blackjack game state with 1 player who has 2 hands, with 2nd hand being current hand")
    val player = 
      BlackjackPlayerState(
        "Avery", 
        20, 
        Seq(Hand(Seq(Card(Seven, Hearts), Card(Ten, Diamonds))), Hand(Seq(Card(Three, Clubs), Card(Nine, Clubs)))))
    val dealerHand = Hand(Seq(Card(Three, Hearts), Card(Eight, Clubs)))
    val state: BlackjackGameState = 
      BlackjackGameState(
        players = Seq(player), 
        dealerHand = dealerHand, 
        currentPlayerIndex = Some(0),
        currentHandIndex = Some(0))
    When("checking whether current hand is player's last hand")
    val isLastHand: Boolean = state.isLastHand()
    Then("isLastHand should be false")
    isLastHand shouldBe (false)
  }

  it should "consider the hand to be the last hand for a player who has 1 hand" in {
    Given("a blackjack game state with 1 player who has 1 hand")
    val player = 
      BlackjackPlayerState(
        "Avery", 
        20, 
        Seq(Hand(Seq(Card(Seven, Hearts), Card(Ten, Diamonds)))))
    val dealerHand = Hand(Seq(Card(Three, Hearts), Card(Eight, Clubs)))
    val state: BlackjackGameState = 
      BlackjackGameState(
        players = Seq(player), 
        dealerHand = dealerHand, 
        currentPlayerIndex = Some(0),
        currentHandIndex = Some(0))
    When("checking whether current hand is player's last hand")
    val isLastHand: Boolean = state.isLastHand()
    Then("isLastHand should be true")
    isLastHand shouldBe (true)
  }

  it should "have knowledge when a player's last 2 outcomes were wins" in {

    pending 
  }

  it should "have knowledge when a player doesn't yet have any outcomes in her history" in {

    pending 
  }

  it should "have knowledge when a player's complete outcome history is win, lose, lose" in {

    pending
  }
}
