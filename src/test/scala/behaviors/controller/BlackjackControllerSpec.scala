package cards.behaviors.controller

import cards.behaviors.Commons
import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.behaviors.betting.BlackjackBetting
import cards.behaviors.controller.BlackjackController
import cards.behaviors.play.BlackjackPlay
import cards.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.classes.DeckType._
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.hand.Hand
import cards.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.classes.options.blackjack.BlackjackOptions
import cards.classes.options.blackjack.BlackjackPayout._
import cards.classes.options.blackjack.DealerHitLimit._
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackControllerSpec extends AnyFlatSpec with GivenWhenThen {
  private [controller] case object _betting extends BlackjackBetting {
    override type EVAL = BlackjackHandEvaluation
    override val evaluation = _evaluation
  }

  private [controller] case object _commons extends Commons
  private [controller] case object _evaluation extends BlackjackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  private [controller] case object _play extends BlackjackPlay {
    override type COMMONS = Commons
    override val commons = _commons
    override type EVAL = BlackjackHandEvaluation
    override val evaluation: EVAL = _evaluation 
  }

  case object module extends BlackjackController {
    override type BETTING = BlackjackBetting
    override type PLAY = BlackjackPlay
    override val betting = _betting
    override val play = _play
  }

  "BlackjackController" should "throw an illegal state exception when proceeding to next state from a game state without any players" in {
    Given("a blackjack game state without any players")
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Nil)
    When("proceeding to the next state")
    Then("an illegal state exception should be thrown")
    an [IllegalStateException] shouldBe thrownBy (module.next(gameState)) 
  }

  "BlackjackController" should "default to first player when proceeding to next state from game with players but with no designated current player" in {
    Given("a blackjack game state with 3 players but with no designated current player")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Four, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        outcome = None)))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(Seq(Card(Two, Clubs), Card(Ace, Spades)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        outcome = None)))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Three, Spades), Card(Seven, Hearts)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        outcome = None)))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(dealerCards), players = Seq(player1, player2, player3), currentPlayerIndex = None)
    When("proceeding to the next state")
    val nextState = module.next(gameState) 
    Then("the first player will have been designated as current player")
    nextState.currentPlayerIndex should equal (Some(0))
  }
  
  it should "throw an illegal state exception when proceeding to next state from a game state whose deck is empty" in {
    Given("a blackjack game state whose deck has no remaining cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Four, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        outcome = None)))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(Seq(Card(Two, Clubs), Card(Ace, Spades)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        outcome = None)))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Three, Spades), Card(Seven, Hearts)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        outcome = None)))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(), 
      deck = Deck.emptyDeck, 
      dealerHand = Hand(dealerCards), 
      players = Seq(player1, player2, player3),
      currentPlayerIndex = Some(0))
    When("proceeding to the next state")
    Then("an illegal state exception should be thrown")
    an [IllegalStateException] shouldBe thrownBy (module.next(gameState)) 
  }

  it should "settle when current hand has busted but dealer doesn't have any cards" in {
    Given("a game state with 1 existing player (Jeffrey) but only Jeffrey has a busted hand dealer doesn't have a hand")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Eight, Hearts), Card(Jack, Diamonds), Card(Five, Clubs)), 
        bets = Map("Jeffrey" -> 15))))//,
        // outcome = Some(BlackjackAction.Lose))))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(), 
      players = Seq(player1),
      currentPlayerIndex = Some(0),
      currentHandIndex = Some(0))
    When("progressing to the next state")
    var result: BlackjackGameState = module.next(gameState, iterations = 1, purgeHistoryAfterRound = false)
    Then("it should be time to settle")
    module.betting.isTimeToSettle(result) shouldBe (true)
    When("progressing to the next state") 
    result = module.next(result, iterations = 1, purgeHistoryAfterRound = false)
    Then("Jeffrey's bank should be less than initial bank")
    val updatedPlayer = result.players.filter(_.id == "Jeffrey").head
    updatedPlayer.bank shouldBe < (25)
    Then("history should reflect that Jeffrey has has Busted")
    // Then("HERE IS HISTORY: " + result.history.mkString("\r\n\r\n"))
    result.history.reverse.head.playerId should equal ("Jeffrey")
    result.history.reverse.head.action should equal (Bust)
    Then("history afterTokens should match Jeffrey's new bank amount")
    result.history.reverse.head.afterTokens should equal (Some(updatedPlayer.bank))
  }
  
  it should "settle when current hand has blackjack but dealer doesn't have any cards" in {
    Given("a game state with 1 existing player (Jeffrey) but only Jeffrey has a 2-card blackjack hand dealer doesn't have a hand")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Ace, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15)))) //, 
        // outcome = Some(BlackjackAction.Win))))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(), 
      players = Seq(player1),
      currentPlayerIndex = Some(0),
      currentHandIndex = Some(0))
    When("progressing to the next state")
    var result: BlackjackGameState = module.next(gameState, iterations = 1, purgeHistoryAfterRound = false)
    Then("it should be time to settle")
    module.betting.isTimeToSettle(result) shouldBe (true)
    When("progressing to the next state") 
    result = module.next(result, iterations = 1, purgeHistoryAfterRound = false)
    Then("Jeffrey's bank should be greater than initial bank") 
    val updatedPlayer = result.players.filter(_.id == "Jeffrey").head
    updatedPlayer.bank shouldBe > (25)
    Then("History should reflect that Jeffrey has Blackjack")
    result.history.reverse.head.playerId should equal ("Jeffrey")
    result.history.reverse.head.action should equal (Blackjack)
    Then("history afterTokens should match Jeffrey's new bank amount")
    result.history.reverse.head.afterTokens should equal (Some(updatedPlayer.bank))
  }

  it should "not allow player or dealer to continue playing after player with 3 card hand has busted" in {
    Given("a game with 1 player having 3 cards and whose hand is busted, a dealer with no cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Ten, Hearts), Card(Eight, Clubs), Card(Four, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val history = Seq(Action("Jeffrey", Hit, Seq(Card(Four, Clubs)), None, Seq(Card(Ten, Hearts), Card(Eight, Clubs)), Seq(Seq(Card(Ten, Hearts), Card(Eight, Clubs), Card(Four, Clubs)))))
    val game = BlackjackGameState(
      options = BlackjackOptions(dealerHitLimit = H17), 
      minimumBet = 5, 
      players = Seq(player1), 
      history = history, 
      currentPlayerIndex = Some(0),
      currentHandIndex = Some(0))
    When("progressing to the next state")
    var result: BlackjackGameState = module.next(game, iterations = 1, purgeHistoryAfterRound = false)
    Then("it should be time to settle")
    module.betting.isTimeToSettle(result) shouldBe (true)
    When("progressing to the next state") 
    result = module.next(result, iterations = 1, purgeHistoryAfterRound = false)
    Then("Jeffrey's bank should be less than initial bank") 
    val updatedPlayer = result.players.filter(_.id == "Jeffrey").head
    updatedPlayer.bank shouldBe < (25)
    Then("round should end with player Busting before Losing")
    result.history.reverse.head.action should equal (Bust)
    result.history.reverse.head.playerId should equal ("Jeffrey")
    Then("history afterTokens should match Jeffrey's new bank amount")
    result.history.reverse.head.afterTokens should equal (Some(updatedPlayer.bank))
  }

  it should "not allow player or dealer to continue playing after player with 3 card hand has achieved 21" in {
    Given("a game with 1 player having 3 cards and whose hand equals 21, a dealer with no cards")
    val hand = Seq(Card(Ten, Hearts), Card(Eight, Clubs), Card(Three, Clubs))
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = hand, bets = Map("Jeffrey" -> 5), outcome = None)) )
    val history = Seq(Action("Jeffrey", Hit, Seq(Card(Four, Clubs)), None, hand, Seq(hand)))
    val game = BlackjackGameState(
      options = BlackjackOptions(dealerHitLimit = H17),
      minimumBet = 5, 
      players = Seq(player1), 
      history = history, 
      currentPlayerIndex = Some(0),
      currentHandIndex = Some(0))
    When("progressing to the next state")
    var result: BlackjackGameState = module.next(game, iterations = 1, purgeHistoryAfterRound = false)
    When("progressing to the next state") 
    result = module.next(result, iterations = 1, purgeHistoryAfterRound = false)
    Then("round should end with player Busting before Losing")
    result.history.reverse.head.playerId should equal ("Jeffrey")
    result.history.reverse.head.action should equal (Win)
  }

  it should "not allow player or dealer to continue playing after player has been dealt a blackjack" in {
    Given("a game with 1 player having 2 cards and whose hand equals 21, a dealer with no cards")
    val hand = Seq(Card(Ten, Hearts), Card(Ace, Clubs))
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25,
      Seq(
        Hand(hand = hand, bets = Map("Jeffrey" -> 5), outcome = None)) )
    val game = BlackjackGameState(
      options = BlackjackOptions(dealerHitLimit = H17), 
      minimumBet = 5, 
      players = Seq(player1),
      currentPlayerIndex = Some(0),
      currentHandIndex = Some(0))
    When("progressing to the next state")
    var result: BlackjackGameState = module.next(game, iterations = 1, purgeHistoryAfterRound = false)
    Then("it should be time to settle")
    module.betting.isTimeToSettle(result) shouldBe (true)
    When("progressing to the next state") 
    result = module.next(result, iterations = 1, purgeHistoryAfterRound = false)
    Then("Jeffrey's bank should be greater than initial bank") 
    val updatedPlayer = result.players.filter(_.id == "Jeffrey").head
    updatedPlayer.bank shouldBe > (25)
    Then("player should have actions Blackjack and Win recorded in history")
    result.history.reverse.head.playerId should equal ("Jeffrey")
    result.history.reverse.head.action should equal (Blackjack)
    Then("history afterTokens should match Jeffrey's new bank amount")
    result.history.reverse.head.afterTokens should equal (Some(updatedPlayer.bank))
  }

  it should "settle when all hands have either won or lost" in {
    Given("a game state with 3 existing players (Jeffrey, Alice, Brandon) who each have 1 or more hands, all of which have either won or lost")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 15, "Alice" -> 10), 
        outcome = Some(BlackjackAction.Lose))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(Seq(Card(Ten, Clubs), Card(Ten, Hearts)), 
        bets = Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15),
        outcome = Some(BlackjackAction.Win))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        Hand(Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 
        bets = Map("Brandon" -> 20, "Alice" -> 25),
        outcome = Some(BlackjackAction.Lose))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(), 
      dealerHand = Hand(dealerCards), 
      players = Seq(player1, player2, player3),
      currentPlayerIndex = Some(0))
    When("progressing to the next state")
    val settledBets: BlackjackGameState = module.next(gameState)
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
  }

  it should "pay blackjack 3-to-2 by default (when not specified as an option)" in {
    Given("a game state with no options specified and with 1 player who's bet 2 on his hand and who's won with a Blackjack")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      20, 
      Seq( 
        Hand(Seq(Card(Ace, Hearts), Card(Jack, Diamonds)), 
        bets = Map("Jeffrey" -> 2), // bet 2 on his hand 
        outcome = Some(BlackjackAction.Win))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(), 
      dealerHand = Hand(dealerCards), 
      players = Seq(player1),
      currentPlayerIndex = Some(0))
    When("progressing to the next state")
    val settledBets = module.next(gameState)  
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
        outcome = Some(BlackjackAction.Win))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(blackjackPayout = SixToFive), 
      dealerHand = Hand(dealerCards), 
      players = Seq(player1),
      currentPlayerIndex = Some(0))
    When("progressing to the next game state")
    val settledBets = module.next(gameState)  
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
        outcome = Some(BlackjackAction.Win))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(blackjackPayout = OneToOne), 
      dealerHand = Hand(dealerCards), 
      players = Seq(player1),
      currentPlayerIndex = Some(0))
    When("progressing to the next state")
    val settledBets = module.next(gameState)  
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
        outcome = Some(BlackjackAction.Lose))))
    val dealerCards: Hand = Hand(Seq(Card(Ace, Diamonds), Card(Ten, Spades)), Map("Jeffrey" -> 1), Nil, Some(BlackjackAction.Win))
    val gameState = BlackjackGameState(
      dealerHand = dealerCards, 
      players = Seq(player1),
      currentPlayerIndex = Some(0))
    When("progressing to the next game state")
    val settledBets = module.next(gameState)  
    Then("player should win 2 for insurance but lose 1 for his losing hand, for a new bank total of 21")
    settledBets.players.head.bank should equal (21) 
  }

  it should "stand on a 3 card hand whose value is 20" in {
    Given("a game with 1 player who has a 3-card hand whose value is twenty")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      20, 
      Seq( 
        Hand(Seq(Card(Two, Hearts), Card(Eight, Diamonds), Card(Ten, Clubs)), 
        bets = Map("Jeffrey" -> 5))))
    val dealerCards: Hand = Hand(Seq(Card(Nine, Diamonds), Card(Ten, Spades)), Map("Jeffrey" -> 1), Nil, None)
    val gameState = BlackjackGameState(
      dealerHand = dealerCards, 
      players = Seq(player1),
      currentPlayerIndex = Some(0),
      currentHandIndex = Some(0))
    When("progressing to the next game state")
    val result = module.next(gameState)  
    Then("the player should Stand")
    val playerHistory = result.history.filter(_.playerId == "Jeffrey")
    playerHistory.reverse.head.playerId should equal ("Jeffrey")
    playerHistory.reverse.head.action should equal (Stand)
  }

}