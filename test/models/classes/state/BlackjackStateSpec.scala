package cards.models.classes.state

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.{ Card, Rank, Suit, Deck, HandBet }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackStateSpec extends AnyFlatSpec with GivenWhenThen {
  "BlackjackPlayerState" should "yield all bets a given player has placed on a specific Blackjack player's hand" in {
    Given("a player state who has placed bet on his own hand and who also has 2 other players who have placed bets on his hand")
    val player = BlackjackPlayerState("Jeffrey", 50, Seq( HandBet(Seq(Card(Ten, Clubs), Card(Jack, Hearts)), Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15))))

    When("retrieving player bets for Jeffrey, Alice, Brandon, and a non-existent Dracula")
    val jeffreyBet: Option[(Seq[Card], Int)] = player.playerBet("Jeffrey")
    val aliceBet: Option[(Seq[Card], Int)] = player.playerBet("Alice")
    val brandonBet: Option[(Seq[Card], Int)] = player.playerBet("Brandon")
    val nonExistentBet: Option[(Seq[Card], Int)] = player.playerBet("Dracula")

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
        HandBet(Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 
        Map("Jeffrey" -> 15, "Alice" -> 10))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        HandBet(Seq(Card(Ten, Clubs), Card(Ace, Spades)), 
        Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        HandBet(Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 
        Map("Brandon" -> 20, "Alice" -> 25))))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Nil, players = Seq(player1, player2, player3))

    When("retrieving player bets for Jeffrey, Alice, Brandon and a non-existent player Santa Claus")
    val jeffreyBets: Seq[(Seq[Card], Int)] = gameState.playerBets("Jeffrey")
    val aliceBets: Seq[(Seq[Card], Int)] = gameState.playerBets("Alice")
    val brandonBets: Seq[(Seq[Card], Int)] = gameState.playerBets("Brandon")
    val nonExistentBets: Seq[(Seq[Card], Int)] = gameState.playerBets("Dracula")
    
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
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Nil, players = Seq(player1, player2, player3))

    When("checking whether it's time to settle bets")
    val settleBets: Boolean = gameState.isTimeToSettle()

    Then("it's determined that it's not yet time to settle any bets")
    settleBets should equal (false)

    When("settling bets")
    val settledBetsSate: BlackjackGameState = gameState.settleBets()

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
        HandBet(Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        handWins = Some(false))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        HandBet(Seq(Card(Ten, Clubs), Card(Ace, Spades)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        handWins = Some(true))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        HandBet(Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        handWins = Some(false))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = dealerCards, players = Seq(player1, player2, player3))

    When("checking whether it's time to settle bets")
    val settleBets: Boolean = gameState.isTimeToSettle()

    Then("it's determined that all bets should be settled")
    settleBets should equal (true)
  
    When("settling bets")
    val settledBets: BlackjackGameState = gameState.settleBets()
    import scala.language.postfixOps
    Then("Jeffrey won 5 and lost 15 from an initial bank of 25 for a total of 15")
    val expectedJeffreyBank: Int = 25 + 5 - 15
    settledBets.players.filter(_.id == "Jeffrey").head.bank should equal (expectedJeffreyBank)
    settledBets.players.filter(_.id == "Jeffrey").head.bank should equal (15)
    Then("Alice won 15 and lost 35 from an initial bank of 50 for a total of 30")
    val expectedAliceBank: Int = 50 + 15 - 25 - 10
    settledBets.players.filter(_.id == "Alice").head.bank should equal (expectedAliceBank)
    settledBets.players.filter(_.id == "Alice").head.bank should equal (30)
    Then("Brandon won 10 and lost 20 from an initial bank of 40 for a total of 30")
    val expectedBrandonBank: Int = 40 + 10 - 20
    settledBets.players.filter(_.id == "Brandon").head.bank should equal (expectedBrandonBank)
    settledBets.players.filter(_.id == "Brandon").head.bank should equal (30)

  }


  it should "not settle when NOT all hands have either won or lost" in {
    Given("a game state with 3 existing players (Jeffrey, Alice, Brandon) who each have 1 or more hands, and all but one of the hands has completed")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        HandBet(Seq(Card(Eight, Hearts), Card(Jack, Diamonds), Card(Five, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        handWins = Some(false))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        HandBet(Seq(Card(Ten, Clubs), Card(Ace, Spades)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        handWins = Some(true))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        HandBet(Seq(Card(Ten, Spades), Card(Three, Hearts), Card(Two, Clubs)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        handWins = None)))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = dealerCards, players = Seq(player1, player2, player3))

    When("checking whether it's time to settle bets")
    val timeToSettleBets: Boolean = gameState.isTimeToSettle()

    Then("it's determined that it's not yet time to settle any bets")
    timeToSettleBets should equal (false)
  
    When("attempting to settle bets")
    val updatedState: BlackjackGameState = gameState.settleBets()
    Then("game state should be unchanged, with no bets settled")
    updatedState should equal (gameState)
  }

  it should "not settle when no hands have either won or lost" in {
    Given("a game state with 3 existing players (Jeffrey, Alice, Brandon) who each have 1 or more hands, none of which have completed play")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        HandBet(Seq(Card(Four, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        handWins = None)))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        HandBet(Seq(Card(Two, Clubs), Card(Ace, Spades)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        handWins = None)))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        HandBet(Seq(Card(Three, Spades), Card(Seven, Hearts)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        handWins = None)))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = dealerCards, players = Seq(player1, player2, player3))

    When("checking whether it's time to settle bets")
    val timeToSettleBets: Boolean = gameState.isTimeToSettle()

    Then("it's determined that it's not yet time to settle any bets")
    timeToSettleBets should equal (false)
    
    When("attempting to settle bets")
    val updatedState: BlackjackGameState = gameState.settleBets()
    Then("game state should be unchanged, with no bets settled")
    updatedState should equal (gameState)
  }

  it should "not settle on a game with no players" in {
    Given("a game state without any players")
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Nil, players = Nil)

    When("checking whether it's time to settle bets")
    val timeToSettleBets: Boolean = gameState.isTimeToSettle()

    Then("it's determined that it's not yet time to settle any bets")
    timeToSettleBets should equal (false)
    
    When("attempting to settle bets")
    val updatedState: BlackjackGameState = gameState.settleBets()
    Then("game state should be unchanged, with no bets settled")
    updatedState should equal (gameState)
  }

}