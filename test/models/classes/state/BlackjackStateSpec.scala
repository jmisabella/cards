package cards.models.classes.state

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.actions.Action
import cards.models.classes.actions.BlackjackAction._
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
    an [IllegalStateException] shouldBe thrownBy (state.currentCards())
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
    val currentHand = state.currentCards()
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
    val player = 
      BlackjackPlayerState(
        "Jeffrey", 
        20, 
        Seq(Hand(Seq(Card(Three, Clubs), Card(Ten, Diamonds)))))
    val dealerHand = Hand(Seq(Card(Three, Hearts), Card(Eight, Clubs)))
    val history = Seq(
      Action("Jeffrey", Win), 
      Action(
        playerId = "Jeffrey", 
        action = Hit, 
        actionCards = Seq(Card(Two, Hearts)), 
        beforeCards = Seq(Card(Three, Clubs), Card(Ten, Diamonds)), 
        afterCards = Seq(Seq(Card(Three, Clubs), Card(Ten, Diamonds), Card(Two, Hearts)))),
      Action(
        playerId = "Jeffrey", 
        action = Stand, 
        actionCards = Nil,
        beforeCards = Seq(Card(Three, Clubs), Card(Ten, Diamonds), Card(Two, Hearts)),
        afterCards = Seq(Seq(Card(Three, Clubs), Card(Ten, Diamonds), Card(Two, Hearts)))),
      Action(
        playerId = "DEALER",
        action = Stand,
        actionCards = Nil,
        beforeCards = dealerHand.hand,
        afterCards = Seq(dealerHand.hand)
      ),
      Action("Jeffrey", Win))  
    val state: BlackjackGameState = 
      BlackjackGameState(
        players = Seq(player), 
        dealerHand = dealerHand,
        history = history,
        currentPlayerIndex = Some(0),
        currentHandIndex = Some(0))
    When("getting winning history for the player Jeffrey")
    val winningHistory = state.winningHistory("Jeffrey")
    Then("winning history would show exactly 2 wins for Jeffrey") 
    winningHistory should have length (2)
    winningHistory should equal (Seq(true, true))
  }

  it should "have knowledge when a player doesn't yet have any outcomes in her history" in {
    Given("game with single player who has 2 cards but has an empty history") 
    val player = 
      BlackjackPlayerState(
        "Alice", 
        20, 
        Seq(Hand(Seq(Card(Three, Clubs), Card(Ten, Diamonds)))))
    val dealerHand = Hand(Seq(Card(Three, Hearts), Card(Eight, Clubs)))
    val history: Seq[Action[BlackjackAction]] = Nil
    val state: BlackjackGameState = 
      BlackjackGameState(
        players = Seq(player), 
        dealerHand = dealerHand,
        history = history,
        currentPlayerIndex = Some(0),
        currentHandIndex = Some(0))
    When("getting winning history for the player Alice")
    val winningHistory = state.winningHistory("Alice")
    Then("winning history would be empty Alice") 
    winningHistory should have length (0)
    winningHistory should equal (Nil)
  }

  "BlackjackPlayerState" should "throw an error at construction time when minBetMultiplier is less than 1" in {
    Given("a minBetMultiplier 0 and a maxBet 100")
    val (minMultiplierA, maxBetA) = (0, Some(100))
    When("constructing a BlackjackPlayerState using the min multiplier of 1 and maxBet of 100")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] should be thrownBy (BlackjackPlayerState("John", 0, Nil, minMultiplierA, maxBetA))
    
    Given("a minBetMultiplier -2 and a maxBet of 100")
    val (minMultiplierB, maxBetB) = (-2, Some(100))
    When("constructing a BlackjackPlayerState using the min multiplier -2 and max bet 100")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] should be thrownBy (BlackjackPlayerState("John", 0, Nil, minMultiplierB, maxBetB))
  } 

  it should "throw an error at construction time when minBetMulplier is 0" in {
    Given("a minBetMultiplier 0 and no maxBet")
    val (minMultiplierA) = 0
    When("constructing a BlackjackPlayerState using the min multiplier 0")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] should be thrownBy (BlackjackPlayerState("John", 0, Nil, minMultiplierA))
  
    Given("a minBetMultiplier -2")
    val (minMultiplierB, maxMultiplierB) = (-2, -2)
    When("constructing a BlackjackPlayerState using the min multiplier -2")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] should be thrownBy (BlackjackPlayerState("John", 0, Nil, minMultiplierB))
  }

  it should "throw an error at construction time when oscarsGoalMultiplier is 0.75" in {
    Given("an oscarsGoalMultiplier of 0.75")
    val oscarsGoalMultiplier = 0.75
    When("constructing a BlackjackPlayerState using the oscar's goal multiplier 0.75")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] should be thrownBy (BlackjackPlayerState(id = "John", bank = 100, oscarsGoalMultiplier = oscarsGoalMultiplier))
  } 

  it should "throw an error at construction time when oscarsGoalMultiplier 0" in {
    Given("an oscarsGoalMultiplier of 0")
    val oscarsGoalMultiplier = 0.0
    When("constructing a BlackjackPlayerState using the oscar's goal multiplier 0")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] should be thrownBy (BlackjackPlayerState(id = "John", bank = 100, oscarsGoalMultiplier = oscarsGoalMultiplier))
  }
 
  it should "throw an error at construction time when oscarsGoalMultiplier -2.0" in {
    Given("an oscarsGoalMultiplier of -2.0")
    val oscarsGoalMultiplier = -2.0
    When("constructing a BlackjackPlayerState using the oscar's goal multiplier -2.0")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] should be thrownBy (BlackjackPlayerState(id = "John", bank = 100, oscarsGoalMultiplier = oscarsGoalMultiplier))
  }

}