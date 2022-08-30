package cards.models.behaviors.payout

import cards.models.behaviors.Commons
import cards.models.behaviors.payout.BlackjackPayout
import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.options.BlackjackPayout._
// import cards.models.classes.options.Surrender._
// import cards.models.classes.options.DealerHitLimit._
// import cards.models.classes.options.ResplitLimit._
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackPayoutSpec extends AnyFlatSpec with GivenWhenThen {
  private [payout] case object _commons extends Commons
  private [payout] case object _evaluation extends BlackjackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  case object module extends BlackjackPayout {
    override type EVAL = BlackjackHandEvaluation
    override val evaluation = _evaluation
  }
  import module._

  "BlackjackPayout" should "yield all bets a given player has placed on a specific Blackjack player's hand" in {
    Given("a player state who has placed bet on his own hand and who also has 2 other players who have placed bets on his hand")
    val player = BlackjackPlayerState("Jeffrey", 50, Seq( Hand(Seq(Card(Ten, Clubs), Card(Jack, Hearts)), Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15))))

    When("retrieving player bets for Jeffrey, Alice, Brandon, and a non-existent Dracula")
    val jeffreyBet: Option[(Seq[Card], Int)] = playerBet(player, "Jeffrey")
    val aliceBet: Option[(Seq[Card], Int)] = playerBet(player, "Alice")
    val brandonBet: Option[(Seq[Card], Int)] = playerBet(player, "Brandon")
    val nonExistentBet: Option[(Seq[Card], Int)] = playerBet(player, "Dracula")

    Then("the player state should retrieve bets from Jeffrey, Alice, and Brandon but wouldn't retrieve any bet from the non-existent Dracula")
    nonExistentBet shouldBe empty  
    jeffreyBet shouldBe defined
    jeffreyBet.get._2 should equal (5) 
    aliceBet shouldBe defined
    aliceBet.get._2 should equal (15)
    brandonBet shouldBe defined
    brandonBet.get._2 should equal (10)
  }

  "BlackjackGameState" should "yield all bets placed by a given player, across all hands on the board" in {
    Given("a game state with 3 existing players (Jeffrey, Alice, Brandon) who each have 1 or more hands and who have placed bets on each others hands")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 
        Map("Jeffrey" -> 15, "Alice" -> 10))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(Seq(Card(Ten, Clubs), Card(Ace, Spades)), 
        Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 
        Map("Brandon" -> 20, "Alice" -> 25))))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Seq(player1, player2, player3))

    When("retrieving player bets for Jeffrey, Alice, Brandon and a non-existent player Santa Claus")
    val jeffreyBets: Seq[(Seq[Card], Int)] = playerBets(gameState, "Jeffrey")
    val aliceBets: Seq[(Seq[Card], Int)] = playerBets(gameState, "Alice")
    val brandonBets: Seq[(Seq[Card], Int)] = playerBets(gameState, "Brandon")
    val nonExistentBets: Seq[(Seq[Card], Int)] = playerBets(gameState, "Dracula")
    
    Then("the game state should retrieve bets from Jeffrey, Alice, and Brandon but wouldn't retrieve any bet from the non-existent Santa Claus")
    nonExistentBets shouldBe empty
    nonExistentBets should have length (0)
    jeffreyBets should have length (2)
    jeffreyBets should contain ((Seq(Card(Ten, Clubs), Card(Ace, Spades)), 5))
    jeffreyBets should contain ((Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 15))
    jeffreyBets.map(_._2).sorted == Seq(5, 15)
    aliceBets should have length (3)
    aliceBets.map(_._2).sorted == Seq(10, 15, 25)
    aliceBets should contain ((Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 25))
    aliceBets should contain ((Seq(Card(Ten, Clubs), Card(Ace, Spades)), 15))
    aliceBets should contain ((Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 10))
    brandonBets should have length (2)
    brandonBets.map(_._2).sorted == Seq(10, 20)
    brandonBets should contain ((Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 20))
    brandonBets should contain ((Seq(Card(Ten, Clubs), Card(Ace, Spades)), 10))
  }

  it should "not settle on a game in which no hands have been dealt and no bets made" in {
    Given("a game state with 3 existing players who do not yet have any hands")
    val player1 = BlackjackPlayerState("Jeffrey", 50, Nil)
    val player2 = BlackjackPlayerState("Alice", 50, Nil)
    val player3 = BlackjackPlayerState("Brandon", 50, Nil)
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Seq(player1, player2, player3))

    When("checking whether it's time to settle bets")
    val shouldSettleBets: Boolean = isTimeToSettle(gameState)

    Then("it's determined that it's not yet time to settle any bets")
    shouldSettleBets shouldBe (false)

    When("settling bets")
    val settledBetsSate: BlackjackGameState = settleBets(gameState)

    Then("players bets would go unchanged: Jeffrey, Alice, and Brandon all at 50")
    settledBetsSate.players(0) should equal (player1) // no change
    settledBetsSate.players(1) should equal (player2) // no change
    settledBetsSate.players(2) should equal (player3) // no change
  }

  it should "settle when all hands have either won or lost" in {
    Given("a game state with 3 existing players (Jeffrey, Alice, Brandon) who each have 1 or more hands, all of which have either won or lost")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        wins = Some(false))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(Seq(Card(Ten, Clubs), Card(Ten, Hearts)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        wins = Some(true))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        wins = Some(false))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(dealerCards), players = Seq(player1, player2, player3))

    When("checking whether it's time to settle bets")
    val shouldSettleBets: Boolean = isTimeToSettle(gameState)

    Then("it's determined that all bets should be settled")
    shouldSettleBets shouldBe (true)
  
    When("settling bets")
    val settledBets: BlackjackGameState = settleBets(gameState)
    import scala.language.postfixOps
    Then("Jeffrey wins 5 and loses 15 from an initial bank of 25 for a total of 15")
    val expectedJeffreyBank: Int = 25 + 5 - 15
    settledBets.players.filter(_.id == "Jeffrey").head.bank should equal (expectedJeffreyBank)
    settledBets.players.filter(_.id == "Jeffrey").head.bank should equal (15)
    Then("Alice wins 15 and loses 35 from an initial bank of 50 for a total of 30")
    val expectedAliceBank: Int = 50 + 15 - 25 - 10
    settledBets.players.filter(_.id == "Alice").head.bank should equal (expectedAliceBank)
    settledBets.players.filter(_.id == "Alice").head.bank should equal (30)
    Then("Brandon wins 10 and loses 20 from an initial bank of 40 for a total of 30")
    val expectedBrandonBank: Int = 40 + 10 - 20
    settledBets.players.filter(_.id == "Brandon").head.bank should equal (expectedBrandonBank)
    settledBets.players.filter(_.id == "Brandon").head.bank should equal (30)
    When("all bets are settled")
    Then("it's no longer time to settle bets")
    isTimeToSettle(settledBets) shouldBe (false)
  }


  it should "not settle when NOT all hands have either won or lost" in {
    Given("a game state with 3 existing players (Jeffrey, Alice, Brandon) who each have 1 or more hands, and all but one of the hands has completed")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Eight, Hearts), Card(Jack, Diamonds), Card(Five, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        wins = Some(false))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(Seq(Card(Ten, Clubs), Card(Ace, Spades)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        wins = Some(true))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Ten, Spades), Card(Three, Hearts), Card(Two, Clubs)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        wins = None)))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(dealerCards), players = Seq(player1, player2, player3))

    When("checking whether it's time to settle bets")
    val timeToSettleBets: Boolean = isTimeToSettle(gameState)

    Then("it's determined that it's not yet time to settle any bets")
    timeToSettleBets should equal (false)
  
    When("attempting to settle bets")
    val updatedState: BlackjackGameState = settleBets(gameState)
    Then("game state should be unchanged, with no bets settled")
    updatedState should equal (gameState)
  }

  it should "not settle when no hands have either won or lost" in {
    Given("a game state with 3 existing players (Jeffrey, Alice, Brandon) who each have 1 or more hands, none of which have completed play")
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
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(dealerCards), players = Seq(player1, player2, player3))
    When("checking whether it's time to settle bets")
    val timeToSettleBets: Boolean = isTimeToSettle(gameState)
    Then("it's determined that it's not yet time to settle any bets")
    timeToSettleBets shouldBe (false)
    When("attempting to settle bets")
    val updatedState: BlackjackGameState = settleBets(gameState)
    Then("game state should be unchanged, with no bets settled")
    updatedState should equal (gameState)
  }

  it should "not settle on a game with no players" in {
    Given("a game state without any players")
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Nil)

    When("checking whether it's time to settle bets")
    val timeToSettleBets: Boolean = isTimeToSettle(gameState)

    Then("it's determined that it's not yet time to settle any bets")
    timeToSettleBets should equal (false)
    
    When("attempting to settle bets")
    val updatedState: BlackjackGameState = settleBets(gameState)
    Then("game state should be unchanged, with no bets settled")
    updatedState should equal (gameState)
  }

  it should "pay blackjack 3-to-2 by default (when not specified as an option)" in {
    Given("a game state with no options specified and with 1 player who's bet 2 on his hand and who's won with a Blackjack")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      20, 
      Seq( 
        Hand(Seq(Card(Ace, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 2), // bet 2 on his hand 
        wins = Some(true))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(dealerCards), players = Seq(player1))
    When("settling bets")
    isTimeToSettle(gameState) shouldBe (true)
    val settledBets = settleBets(gameState)  
    Then("the player should be paid 3-to-2, so would win 3")
    settledBets.players.head.bank should equal (23) 
  }

  it should "pay blackjack 6-to-5 when specified to do so in blackjack options" in {
    Given("a game state with 6-to-5 option specified and with 1 player who's bet 5 on his hand and who's won with a Blackjack")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      30, 
      Seq( 
        Hand(Seq(Card(Ace, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 5), // bet 2 on his hand 
        wins = Some(true))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(payout = SixToFive), dealerHand = Hand(dealerCards), players = Seq(player1))
    When("settling bets")
    isTimeToSettle(gameState) shouldBe (true)
    val settledBets = settleBets(gameState)  
    Then("the player should be paid 6-to-5, so would win 6 since bet was 5")
    settledBets.players.head.bank should equal (36) 
  }

  it should "pay blackjack 1-to-1 when specified to do so in blackjack options" in {
    Given("a game state with payout option of Blackjack 1-to-1 and with 1 player who's bet 2 on his hand and who's won with a Blackjack")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      20, 
      Seq( 
        Hand(Seq(Card(Ace, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 2), // bet 2 on his hand 
        wins = Some(true))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(payout = OneToOne), dealerHand = Hand(dealerCards), players = Seq(player1))
    When("settling bets")
    isTimeToSettle(gameState) shouldBe (true)
    val settledBets = settleBets(gameState)  
    Then("the player should be paid 1-to-1, so would win 2")
    settledBets.players.head.bank should equal (22) 
  }

  it should "pay insurance 2-to-1 when player has no other bets" in {
    Given("a game state with 1 player who has placed bet of 1 on his (losing) hand, and has also purchased 1 for insurance, and the dealer's hand showing an Ace")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      20, 
      Seq( 
        Hand(Seq(Card(Two, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 1), // bet 2 on his hand 
        wins = Some(false))))
    val dealerCards: Hand = Hand(Seq(Card(Ace, Diamonds), Card(Ten, Spades)), Map("Jeffrey" -> 1), Some(true))
    val gameState = BlackjackGameState(dealerHand = dealerCards, players = Seq(player1))
    When("settling bets")
    isTimeToSettle(gameState) shouldBe (true)
    val settledBets = settleBets(gameState)  
    Then("player should win 2 for insurance but lose 1 for his losing hand, for a new bank total of 21")
    settledBets.players.head.bank should equal (21) 
  }
}
