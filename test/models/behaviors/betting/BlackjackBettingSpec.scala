package cards.models.behaviors.betting

import cards.models.behaviors.Commons
import cards.models.behaviors.betting.BlackjackBetting
import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.bettingstrategy.BlackjackBettingStrategy._
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.actions.Action
import cards.models.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackBettingSpec extends AnyFlatSpec with GivenWhenThen {
  private [betting] case object _commons extends Commons
  private [betting] case object _evaluation extends BlackjackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  case object module extends BlackjackBetting {
    override type EVAL = BlackjackHandEvaluation
    override val evaluation = _evaluation
  }
  import module._

  "BlackjackBetting" should 
  "yield all bets a given player has placed on a specific Blackjack player's hand" in {
    Given("a player state who has placed bet on his own hand and who also has 2 other players who have placed bets on his hand")
    val player = BlackjackPlayerState("Jeffrey", 50, Seq( Hand(Seq(Card(Ten, Clubs), Card(Jack, Hearts)), Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15))))

    When("retrieving player bets for Jeffrey, Alice, Brandon, and a non-existent Dracula")
    val jeffreyBet: Option[(Seq[Card], Int)] = getPlayerBet(player, "Jeffrey")
    val aliceBet: Option[(Seq[Card], Int)] = getPlayerBet(player, "Alice")
    val brandonBet: Option[(Seq[Card], Int)] = getPlayerBet(player, "Brandon")
    val nonExistentBet: Option[(Seq[Card], Int)] = getPlayerBet(player, "Dracula")

    Then("the player state should retrieve bets from Jeffrey, Alice, and Brandon but wouldn't retrieve any bet from the non-existent Dracula")
    nonExistentBet shouldBe empty  
    jeffreyBet shouldBe defined
    jeffreyBet.get._2 should equal (5) 
    aliceBet shouldBe defined
    aliceBet.get._2 should equal (15)
    brandonBet shouldBe defined
    brandonBet.get._2 should equal (10)
  }

  "BlackjackGameState" should 
  "yield all bets placed by a given player, across all hands on the board" in {
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
    val player3: BlackjackPlayerState = cards.models.classes.state.BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 
        Map("Brandon" -> 20, "Alice" -> 25)))) 
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Seq(player1, player2, player3))

    When("retrieving player bets for Jeffrey, Alice, Brandon and a non-existent player Santa Claus")
    val jeffreyBets: Seq[(Seq[Card], Int)] = getPlayerBets(gameState, "Jeffrey")
    val aliceBets: Seq[(Seq[Card], Int)] = getPlayerBets(gameState, "Alice")
    val brandonBets: Seq[(Seq[Card], Int)] = getPlayerBets(gameState, "Brandon")
    val nonExistentBets: Seq[(Seq[Card], Int)] = getPlayerBets(gameState, "Dracula")
    
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

  it should 
  "not settle on a game in which no hands have been dealt and no bets made" in {
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

  it should 
  "settle when all hands have either won or lost" in {
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
    val gameState = BlackjackGameState(options = BlackjackOptions(blackjackPayout = SixToFive), dealerHand = Hand(dealerCards), players = Seq(player1))
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
    val gameState = BlackjackGameState(options = BlackjackOptions(blackjackPayout = OneToOne), dealerHand = Hand(dealerCards), players = Seq(player1))
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

  it should "know it's time to take new bets when no players have any cards" in {
    Given("a game with 2 players, neither of whom have any cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Nil)
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Nil)
    val gameState = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1, player2))
    When("checking whether it's time to take new bets")
    Then("it's determined that yes, it is in fact time to take new bets")
    isTimeToPlaceNewBets(gameState) shouldBe (true)
  }

  it should "know it's no longer time to take new bets (except for double downs and insurance) when players have 3 cards" in {
    Given("a game with 2 players, each having 3 cards, and the dealer who also has 2 cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq(Hand(Seq(Card(Two, Clubs), Card(Four, Diamonds), Card(Nine, Hearts)))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq(Hand(Seq(Card(Ace, Clubs), Card(Ten, Hearts), Card(Four, Spades)))))
    val dealerHand = Hand(Seq(Card(Eight, Clubs), Card(Ace, Diamonds)))   
    val gameState = BlackjackGameState(dealerHand = dealerHand, players = Seq(player1, player2))
    When("checking whether it's time to take new bets")
    Then("it's determined that it is not currently time to accept new bets")
    isTimeToPlaceNewBets(gameState) shouldBe (false)
  }

  it should "throw an illegal argument exception when attempting to place bet on a game with no players" in {
    Given("a game with no players")
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Nil)
    When("placing a new bet")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (placeBet(game))
    the [IllegalArgumentException] thrownBy (placeBet(game)) should have message "Cannot place bet because there are no players"
  }

  it should "throw an illegal argument exception when attempting to place bet on a game with no designated current player" in {
    Given("a game with 2 players, each with no cards, but neither of whom designated as the current player")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Nil)
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Nil)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1, player2), currentPlayerIndex = None)
    When("checking whether it's time to accept new bets")
    Then("it's determined that it's time to accept new bets")
    isTimeToPlaceNewBets(game) shouldBe (true)
    When("placing a new bet")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (placeBet(game))
    the [IllegalArgumentException] thrownBy (placeBet(game)) should have message "Cannot place bet because no player is designated as the current player"
  }

  it should "throw an illegal argument exception when attempting to place bet on a game when it is not the proper time to accept new bets" in {
    Given("a game with 2 players, each having 2 or more cards, one of whom the designated current player")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq(Hand(Seq(Card(Two, Clubs), Card(Four, Diamonds), Card(Nine, Hearts)))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq(Hand(Seq(Card(Ace, Clubs), Card(Ten, Hearts), Card(Four, Spades)))))
    val dealerHand = Hand(Seq(Card(Eight, Clubs), Card(Ace, Diamonds)))   
    val game = BlackjackGameState(dealerHand = dealerHand, players = Seq(player1, player2), currentPlayerIndex = Some(0))
    When("checking the current player")
    Then("then it's determined the current player is Jeffrey")
    game.currentPlayer() should equal (player1)
    When("checking whether it's time to accept new bets")
    Then("it's determined that it's not time to accept new bets")
    isTimeToPlaceNewBets(game) shouldBe (false)
    When("placing a new bet")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (placeBet(game))
    the [IllegalArgumentException] thrownBy (placeBet(game)) should have message "Cannot place new bets as it is not currently time to take new bets"
  }

  it should "get a player's personal min bet multplier is 1 and max bet is 1 and table's minimum bet is 1" in {
    Given("a game with minimum bet 1 and a player whose min multiplier and max bet are both 1")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 25, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(1))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 1)
    When("getting min and max bet for the player")
    val (minBet, maxBet) = getMinAndMaxBet(player1, game)
    Then("min bet is 1")
    minBet should equal (1)
    Then("max bet is 1")
    maxBet should equal (1)
  }

  it should "get player's personal min bet as table's minimum bet and maximum bet as twice the table's minimum bet when min and max multiplers are set to 1 and 2, respectively" in {
    Given("a game with minimum bet 25 and a player whose min bet multipler is 1 and max bet multiplier is 2")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 200, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(50))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25)
    When("getting min and max bet for the player")
    val (minBet, maxBet) = getMinAndMaxBet(player1, game)
    Then("min bet is 25")
    minBet should equal (25)
    Then("max bet is 50")
    maxBet should equal (50)
  }

  it should 
  "get player's personal max bet as the table's maximum allowed bet when product of table's minimum bet by the player's configured max bet exceeds the table's maximum bet" in {
    Given("a game with minimum bet 25 and maximum bet 200 and a player whose max bet is 250")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(250))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200)
    When("getting min and max bet for the player")
    val (minBet, maxBet) = getMinAndMaxBet(player1, game)
    Then("player's minimum bet is 25")
    minBet should equal (25)
    Then("player's maximum bet is 200")
    maxBet should equal (200)
  }

  // Negative-Progression betting strategy
  "a player employing Negative-Progression betting strategy using BlackjackBetting" should 
  "place a player's personal minimum bet when the player's history has no wins or losses" in {
    Given(
      "a game with minimum bet 25 and a current player without any wins or losses and who employs the negative-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, history = Nil, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25) 
  }

  it should 
  "place a player's personal minimum bet when the player's history has only wins but no losses" in {
    Given(
      "a game with minimum bet 25 and a current player a single win and who employs the negative-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(Action("Jeffrey", Win)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25) 
  }

  it should 
  "place twice a player's personal minimum when player's last hand lost but previous hand won, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last hand lost, prior to a win, and who employs the negative-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 50 (2 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 50))
    mostRecentBet(player1, betPlaced) should equal (50) 
  }

  it should 
  "place twice a player's personal minimum when player's last 2 hands lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 2 hands lost, prior to a win, and who employs the negative-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 50 (2 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 50))
    mostRecentBet(player1, betPlaced) should equal (50) 
  }

  it should 
  "place three times a player's personal minimum when player's last 3 hands lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 3 hands lost, prior to a win, and who employs the negative-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 75 (3 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 75))
    mostRecentBet(player1, betPlaced) should equal (75)
  }

  it should 
  "place three times a player's personal minimum when player's last 4 hands lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 4 hands lost, prior to a win, and who employs the negative-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 75 (3 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 75))
    mostRecentBet(player1, betPlaced) should equal (75)
  }

  it should 
  "place four times a player's personal minimum when player's last 5 hands lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 5 hands lost, prior to a win, and who employs the negative-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(
      Action("Jeffrey", Win), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 100 (4 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 100))
    mostRecentBet(player1, betPlaced) should equal (100)
  }

  it should 
  "place table's max limit as player's bet when player's last 4 hands lost, and four times player's personal min bet exceeds the table's max limit" in {
    Given(
      "a game with minimum bet 250, a maximum bet of 999, and a current player whose last 5 hands lost, prior to a win, and who employs the negative-progression " + 
      "betting strategy, and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(
      Action("Jeffrey", Win), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 250, maximumBet = 999, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 999 (since 4 * the minimum 250 product of 1000 would exceed table maximum bet of 999)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 999))
    mostRecentBet(player1, betPlaced) should equal (999)
  }

  it should "place a player's personal minimum when player's last hand won but player had lost 4 hands previous to the win" in {
    Given(
      "a game with minimum bet 250, a maximum bet of 999, and a current player last hand won, prior to losing 5 hands consecutively" + 
      "and who employs the negative-progression betting strategy, and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = NegativeProgression)
    val history = Seq(
      Action("Jeffrey", Win), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Lose),
      Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 250, maximumBet = 999, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the minimum amount of 250")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 250))
    mostRecentBet(player1, betPlaced) should equal (250)
  }


  // Martingale betting strategy
  "a player employing Martingale betting strategy using BlackjackBetting" should 
  "place a player's personal minimum bet when the player's history has no wins or losses" in {
    Given(
      "a game with minimum bet 25 and a current player without any wins or losses and who employs the Martingale betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Martingale)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, history = Nil, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should "place a player's personal minimum bet when the player's history has only wins but no losses" in {
    Given(
      "a game with minimum bet 25 and a current player with 2 wins and who employs the Martingale betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Martingale)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should "place twice a player's personal minimum when player's last hand lost but previous hand won, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player with 1 loss prior to 1 win and who employs the Martingale betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Martingale)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 10000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 50 (2 * minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 50))
    mostRecentBet(player1, betPlaced) should equal (50)
  }

  it should "place four times a player's personal minimum when player's last 2 hands lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player with 2 losses prior to 1 win and who employs the Martingale betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Martingale)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 100 (4 * minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 100))
    mostRecentBet(player1, betPlaced) should equal (100)
  }

  it should "place eight times a player's personal minimum when player's last 3 hands lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player with 3 losses prior to 1 win and who employs the Martingale betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Martingale)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 200 (8 * minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 200))
    mostRecentBet(player1, betPlaced) should equal (200)
  }

  it should "place sixteen times a player's personal minimum when player's last 4 hands lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player with 4 losses prior to 1 win and who employs the Martingale betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(1000),
      bettingStrategy = Martingale)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 400 (16 * minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 400))
    mostRecentBet(player1, betPlaced) should equal (400)
  }

  it should "place a player's personal minimum when player's last hand won but player had lost 4 hands previous to the win" in {
    Given(
      "a game with minimum bet 25 and a current player who won last hand after losing 4 hands and who employs the Martingale betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Martingale)
    val history = Seq(Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 25 (the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }
  
  it should "place table's max limit as player's bet when player's last 4 hands lost, and four times player's personal min bet exceeds the table's max limit" in {
    Given(
      "a game with minimum bet 25, a maximum bet 200, and a current player with 4 losses 1 win and who employs the " + 
      "Martingale betting strategy, and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Martingale)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 200 (since 16 * minimum exceeds the maximum bet of 200)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 200))
    mostRecentBet(player1, betPlaced) should equal (200)
  }

  // Positive-Progression betting strategy
  "a player employing Positive-Progression betting strategy using BlackjackBetting" should 
  "place a player's personal minimum bet when the player's history has no wins or losses" in {
    Given(
      "a game with minimum bet 25 and a current player without any wins or losses and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = PositiveProgression)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, history = Nil, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum bet when the player's history has only losses but no wins" in {
    Given(
      "a game with minimum bet 25 and a current player a single loss and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(Action("Jeffrey", Lose)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place twice a player's personal minimum when player's last hand won but previous hand lost, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last hand won, prior to a loss, and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(Action("Jeffrey", Lose), Action("Jeffrey", Win)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 50 (2 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 50))
    mostRecentBet(player1, betPlaced) should equal (50)
  }

  it should 
  "place twice a player's personal minimum when player's last 2 hands won, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 2 hands won, prior to a loss, and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(Action("Jeffrey", Lose), Action("Jeffrey", Win), Action("Jeffrey", Win)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 50 (2 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 50))
    mostRecentBet(player1, betPlaced) should equal (50)
  }

  it should 
  "place three times a player's personal minimum when player's last 3 hands won, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 3 hands won, prior to a loss, and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(Action("Jeffrey", Lose), Action("Jeffrey", Win), Action("Jeffrey", Win), Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 75 (3 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 75))
    mostRecentBet(player1, betPlaced) should equal (75)
  }

  it should 
  "place three times a player's personal minimum when player's last 4 hands won prior to a loss, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 4 hands won, prior to a loss, and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(Action("Jeffrey", Lose), Action("Jeffrey", Win), Action("Jeffrey", Win), Action("Jeffrey", Win), Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 75 (3 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 75))
    mostRecentBet(player1, betPlaced) should equal (75)
  }

  it should 
  "place four times a player's personal minimum when player's last 5 hands won prior to a loss, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 5 hands won, prior to a loss, and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 100 (4 * the minimum)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 100))
    mostRecentBet(player1, betPlaced) should equal (100)
  }

  it should 
  "place table's max limit as player's bet when player's last 4 hands won prior to a loss, and four times player's personal min bet exceeds the table's max limit" in {
    Given(
      "a game with minimum bet 250, a maximum bet of 999, and a current player whose last 5 hands won, prior to a loss, and who employs the positive-progression " + 
      "betting strategy, and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 250, maximumBet = 999, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet 999 (since 4 * the minimum 250 product of 1000 would exceed table maximum bet of 999)")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 999))
    mostRecentBet(player1, betPlaced) should equal (999)
  }

  it should "place a player's personal minimum when player's last hand lost but player had won 4 hands previous to the loss" in {
    Given(
      "a game with minimum bet 250, a maximum bet of 999, and a current player last hand loss, prior to winning 5 hands consecutively" + 
      "and who employs the positive-progression betting strategy, and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = PositiveProgression)
    val history = Seq(
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 250, maximumBet = 999, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the minimum amount of 250")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 250))
    mostRecentBet(player1, betPlaced) should equal (250)
  }

  // Steady betting strategy (always bet the same amount no matter what the outcome) 
  "a player employing Steady betting strategy using BlackjackBetting" should 
  "place a player's personal minimum bet when the player's history has no wins or losses" in {
    Given(
      "a game with minimum bet 25 and a current player without any wins or losses and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = Some(200),
      bettingStrategy = Steady)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, history = Nil, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum bet when the player's history has only wins but no wins" in {
    Given(
      "a game with minimum bet 25 and a current player two wins only and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = Steady)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Win)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }
  
  it should 
  "place a player's personal minimum bet when the player's history has only losses but no wins" in {
    Given(
      "a game with minimum bet 25 and a current player a single loss and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = Steady)
    val history = Seq(Action("Jeffrey", Lose)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum when player's last hand won but previous hand lost" in {
    Given(
      "a game with minimum bet 25 and a current player whose last hand won, prior to a loss, and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = Steady)
    val history = Seq(Action("Jeffrey", Lose), Action("Jeffrey", Win)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum when player's last 2 hands lost prior to a win, and the bet would not exceed player's max limit" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 2 hands lost prior to a win, and who employs the positive-progression betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = Steady)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose)) 
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum when player's last 3 hands won" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 3 hands won, prior to a loss, and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None,
      bettingStrategy = Steady)
    val history = Seq(Action("Jeffrey", Lose), Action("Jeffrey", Win), Action("Jeffrey", Win), Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum when player's last 4 hands lost prior to a win" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 4 hands lost, prior to a win, and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Steady)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum when player's last 5 hands won prior to a loss" in {
    Given(
      "a game with minimum bet 25 and a current player whose last 5 hands won, prior to a loss, and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Steady)
    val history = Seq(
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win), 
      Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 1000, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should "place a player's personal minimum when player's last hand lost but player had won 4 hands previous to the loss" in {
    Given(
      "a game with minimum bet 250, a maximum bet of 999, and a current player last hand loss, prior to winning 5 hands consecutively" + 
      "and who employs the steady betting strategy, and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Steady)
    val history = Seq(
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Win),
      Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 250, maximumBet = 999, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the minimum amount of 250")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 250))
    mostRecentBet(player1, betPlaced) should equal (250)
  }

  // Oscar's betting strategy
  "a player employing Oscar's betting strategy using BlackjackBetting" should 
  "place a player's personal minimum bet when the player's history has no wins or losses" in {
    Given(
      "a game with minimum bet 25 and a current player without any wins or losses and who employs the steady betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Oscars)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, history = Nil, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25, since his minimum multiplier is 1")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    betPlaced.players.filter(_.id == "Jeffrey").head.handsAndBets.map(_.bets("Jeffrey")).head should equal (25)
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum bet when the player's history has a single loss" in {
    Given(
      "a game with minimum bet 25 and a current player without any wins or losses and who employs Oscar's betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Oscars)
    val history = Seq(Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should 
  "place a player's personal minimum bet when the player's history has 2 losses prior to a win" in {
    Given(
      "a game with minimum bet 25 and a current player without any wins or losses and who employs Oscar's betting strategy, " + 
      "and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Oscars)
    val history = Seq(Action("Jeffrey", Win), Action("Jeffrey", Lose), Action("Jeffrey", Lose))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should bet the table minimum of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 25))
    mostRecentBet(player1, betPlaced) should equal (25)
  }

  it should
  "double last bet when last hand won, provided doubling the bet doesn't exceed table's max and that Oscar's goal has not yet been reached" in {
    Given(
      "a game with a minimum bet of 25 and a maximum of 200, with a current player employing Oscar's betting strategy and whose previous hand won " + 
      "and double his previous bet does not exceed table's max, and whose next Oscar's goal has not yet been reached and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Oscars,
      oscarsGoalMultiplier = 1.25,
      oscarsGoal = (1.25 * 2000).toInt)
    val history: Seq[Action[BlackjackAction]] = Seq(
      Action("Jeffrey", action = Bet, actionTokens = 25), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, history = history, currentPlayerIndex = Some(0))
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should have bet 50, twice his last bet of 25")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 50))
    mostRecentBet(player1, betPlaced) should equal (50)
  }

  it should
  "bet max bet when last hand won, provided doubling last bet exceeds table's max and that Oscar's goal has not yet been reached" in {
    Given(
      "a game with a minimum bet of 25 and a maximum of 200, with a current player employing Oscar's betting strategy and whose previous hand won " + 
      "and double his last bet of 125 exceeds the table's max, and whose next Oscar's goal has not yet been reached and it's time to take new bets")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Oscars,
      oscarsGoalMultiplier = 1.25,
      oscarsGoal = (1.25 * 2000).toInt)
    val history: Seq[Action[BlackjackAction]] = Seq(
      Action("Jeffrey", action = Bet, actionTokens = 125), 
      Action("Jeffrey", Lose), 
      Action("Jeffrey", Win),
      Action("Jeffrey", Win))
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, history = history, currentPlayerIndex = Some(0))
    When("checking whether it's time to accept new bets")
    Then("it's determined that it's time to accept new bets")
    isTimeToPlaceNewBets(game) shouldBe (true)
    When("placing his bet")
    val betPlaced = placeBet(game)
    Then("player should have bet 200, the table's max, since doubling his last bet of 125 would have exceeded the table's max")
    betPlaced.history.length shouldBe >= (1) 
    betPlaced.history.reverse.head should equal (Action("Jeffrey", Bet, Nil, 200))
    mostRecentBet(player1, betPlaced) should equal (200)
  }

  // TEST GROUPING: determine when a player should increase or decrease his or her minimum bet multiplier, and by how much
  "BlackjackBetting: a player" should 
  "increase his or her minimum bet by increasing min bet multiplier by 1, when 25 games have been completed and the player's bank has increased" in {
    Given("a game with a player whose bank has increased after 25 games and whose personal minimum bet is less than the table's max bet")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 2,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 25,
      bankEvery25Hands = 1900)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should increase their personal minimum bet")
    val alteredMinBet: BlackjackGameState = alterMinBet(player1, game)
    Then("player's minimum bet should be increased by doubling it")
    val updatedPlayer1: BlackjackPlayerState = alteredMinBet.players.filter(_.id == "Jeffrey").head
    updatedPlayer1.minBetMultiplier should equal (player1.minBetMultiplier + 1)
  }

  it should
  "throw an illegal argument exception when checking to alter minimum bet for player who does not belong to the game" in {
    Given("a game and a player who does not belong to the game")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 2,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 25,
      bankEvery25Hands = 1900)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Nil, minimumBet = 25, maximumBet = 200)
    When("checking whether player should alter their minimum bet")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (alterMinBet(player1, game))
  }

  it should 
  "not increase his or her minimum bet by by increasing min bet multiplier by 1 it when only 24 games have been completed and the player's bank has increased" in {
    Given("a game with a player whose bank has increased after 24 games and whose personal minimum bet is less than the table's max bet")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 2,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 24,
      bankEvery25Hands = 1900)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should increase their personal minimum bet")
    val alteredMinBet: BlackjackGameState = alterMinBet(player1, game)
    Then("player's minimum bet should stay the same")
    val updatedPlayer1: BlackjackPlayerState = alteredMinBet.players.filter(_.id == "Jeffrey").head
    updatedPlayer1.minBetMultiplier should equal (player1.minBetMultiplier)
  }
  
  it should 
  "not increase his or her minimum bet when 25 games have been completed and the player's bank has increased " + 
  "but is already betting at the table's maximum bet" in {
    Given("a game with a player whose bank has increased after 25 games and whose personal minimum bet the same as the table's max bet")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 10,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 25,
      bankEvery25Hands = 1900)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should increase their personal minimum bet")
    val alteredMinBet: BlackjackGameState = alterMinBet(player1, game)
    Then("player's minimum bet should stay the same")
    val updatedPlayer1: BlackjackPlayerState = alteredMinBet.players.filter(_.id == "Jeffrey").head
    updatedPlayer1.minBetMultiplier should equal (player1.minBetMultiplier)
  }
  
  it should "decrease his or her minimum bet by by decreasing min bet multiplier by 1 when 75 games have been completed and the player's bank " + 
  "has decreased, provided player's minimum bet is not already at the table's minimum" in {
    Given("a game with a player whose bank has decreased after 75 games and whose personal minimum bet is greater than the table's min bet")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 3,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 75,
      bankEvery25Hands = 3200)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should decrease their personal minimum bet")
    val alteredMinBet: BlackjackGameState = alterMinBet(player1, game)
    Then("player's minimum bet multiplier should be decreased by 1")
    val updatedPlayer1: BlackjackPlayerState = alteredMinBet.players.filter(_.id == "Jeffrey").head
    updatedPlayer1.minBetMultiplier should equal (player1.minBetMultiplier - 1)
  }

  it should "not decrease his or her minimum bet when 50 games have been completed, player's hand has decreased, " + 
  "but is already betting at the table's minimum" in {
    Given("a game with a player whose bank has decreased after 50 games and whose personal minimum bet is the same as the table's min bet")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 2000, 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 50,
      bankEvery25Hands = 3200)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should decrease their personal minimum bet")
    val alteredMinBet: BlackjackGameState = alterMinBet(player1, game)
    Then("player's minimum bet should remain the same, at the table's minimum")
    val updatedPlayer1: BlackjackPlayerState = alteredMinBet.players.filter(_.id == "Jeffrey").head
    updatedPlayer1.minBetMultiplier should equal (player1.minBetMultiplier)
  }

  // TEST GROUPING: determine when a player is not doing well with a betting strategy and should switch to a different betting strategy
  it should "change betting strategies to a random strategy if, after 250 games have been completed " + 
  "and player's bank has not increased by 15%" in {
    Given("a game with a player whose bank has not increased by 15% after 250 games")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 1149, // less than 15% of 1000 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 250,
      bankEvery250Hands = 1000)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should change to a different betting strategy")
    val alteredStrategy: BlackjackGameState = alterBettingStrategy(player1, game) 
    Then("player's betting strategy should randomly change to a different strategy")
    val updatedPlayer1: BlackjackPlayerState = alteredStrategy.players.filter(_.id == "Jeffrey").head
    updatedPlayer1.bettingStrategy should not equal (player1.bettingStrategy)
  }

  it should 
  "throw an illegal argument exception when checking whether to change a player's betting strategy when the player does not belong to the game" in {
    Given("a player who hasn't increased bank by 15% in 250 games but who does not belong to the specified game")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 1149, // less than 15% of 1000 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 250,
      bankEvery250Hands = 1000)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Nil, minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should change to a different betting strategy")
    Then("an illegal exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (alterBettingStrategy(player1, game))
  }

  it should "not change betting strategies if 250 games have been completed and player's bank has increased by 15%" in {
    Given("a game with a player whose bank has increased by 15% after 250 games")
    val player1 = BlackjackPlayerState(
      id = "Jeffrey", 
      bank = 1150, // 15% of 1000 
      handsAndBets = Nil,
      minBetMultiplier = 1,
      maxBet = None: Option[Int],
      bettingStrategy = Steady,
      completedHands = 250,
      bankEvery250Hands = 1000)
    val game = BlackjackGameState(dealerHand = Hand.empty, players = Seq(player1), minimumBet = 25, maximumBet = 200, currentPlayerIndex = Some(0))
    When("checking whether player should change to a different betting strategy")
    val alteredStrategy: BlackjackGameState = alterBettingStrategy(player1, game) 
    Then("player's betting strategy should remain the same strategy")
    val updatedPlayer1: BlackjackPlayerState = alteredStrategy.players.filter(_.id == "Jeffrey").head
    updatedPlayer1.bettingStrategy should equal (player1.bettingStrategy)
  }

  it should "not allow players insurance if dealer's hand is an empty list" in {
    val dealerCards: Seq[Card] = Nil
    val result: Boolean = eligibleForInsurance(dealerCards)
    result should be (false)
  }
  
  it should "not allow players insurance if dealer's hand has more than 1 card" in {
    val dealerCards: Seq[Card] = Seq(Card(Ace, Spades), Card(Seven, Hearts), Card(Three, Spades)) 
    val result: Boolean = eligibleForInsurance(dealerCards)
    result should be (false)
  }

  it should "allow players insurance if dealer's hand has exactly 2 cards and its face-up card (second card) is an Ace" in {
    val dealerCards: Seq[Card] = Seq(Card(Two, Clubs), Card(Ace, Hearts))
    val result: Boolean = eligibleForInsurance(dealerCards)
    result should be (true)
  }



}
