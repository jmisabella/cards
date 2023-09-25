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
import cards.classes.bettingstrategy.BlackjackBettingStrategy._
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

  it should "have history reflect when dealer has busted or achieved 21 for a hand of 3 cards" in {
    Given("a game with 1 player who Stands on a 3-card hand with score of 18 and it's dealer's turn and dealer has a hand of 16")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      20, 
      Seq( 
        Hand(Seq(Card(Two, Hearts), Card(Eight, Diamonds), Card(Eight, Clubs)), 
        bets = Map("Jeffrey" -> 5))))
    val playerHistory: Seq[Action[BlackjackAction]] = Seq(Action("Jeffrey", Stand))
    val dealerCards: Hand = Hand(Seq(Card(Six, Diamonds), Card(Ten, Spades)), Map(), Nil, None)
    val game = BlackjackGameState(
      dealerHand = dealerCards, 
      players = Seq(player1),
      currentPlayerIndex = None,
      currentHandIndex = None)
    module.play.isTimeForDealerToPlay(game) shouldBe (true)
    var result = module.next(game, iterations = 1, purgeHistoryAfterRound = false)
    var dealerScore: Long = module.play.evaluation.eval(result.dealerHand.hand)
    if (dealerScore > 21) {
      When("dealer's hand exceeds 21")
      Then("history should show dealer has Busted")
      result.history.count(a => a.playerId.toLowerCase == "dealer" && a.action == Bust) shouldBe > (0)
    } else if (dealerScore == 21) {
      When("dealer's hand reaches 21")
      Then("history should show dealer hitting")
      result.history.count(a => a.playerId.toLowerCase == "dealer" && a.action == Hit) shouldBe > (0)
      Then("history should show dealer showing cards")
      result.history.count(a => a.playerId.toLowerCase == "dealer" && a.action == ShowCards) shouldBe > (0)
      // Then("history should not show dealer standing")
      // result.history.count(a => a.playerId.toLowerCase == "dealer" && a.action == Stand) should equal (0)
    }
  }

  it should "init a new single player game with both player and dealer initial ranks to be overridden" in {
    Given("a BlackjackOptions which specifies player initial ranks override of [Two, Two] and dealer initial rank overrides of [Three, Ace]")
    val options = BlackjackOptions(
      deckCount = 1,
      dealerHitLimit = S17,
      blackjackPayout = ThreeToTwo,
      allowSurrender = true,
      hitOnSplitAces = true,
      resplitOnSplitAces = true,
      initialBank = 200, 
      playerInitialRanks = Seq(Two, Two), 
      dealerInitialRanks = Seq(Three, Ace)) 
    When("initializing a new 1-player game") 
    val game = module.init(1, options)
    Then("game players should be length 1 and whose only player has playerId 'player1'")
    game.players should have length (1)
    game.players.head.id should equal ("player1")
    Then("deck should reflect that 2 Twos have been dealt")
    game.deck.count(_.rank == Two) should equal (2)
    Then("deck should reflect that 1 Three has been dealt")
    game.deck.count(_.rank == Three) should equal (3)
    Then("deck should reflect that 1 Ace has been dealt")
    game.deck.count(_.rank == Ace) should equal (3)
    Then("player hand should reflect that it contains a pair of Twos")
    game.players should have length (1)
    game.players.head.hands should have length (1)
    game.players.head.hands.head should have length (2)
    game.players.head.hands.head.head.rank should equal (Two)
    game.players.head.hands.head.tail.head.rank should equal (Two)
    Then("dealer hand should reflect that it contains a Three and an Ace")
    game.dealerHand.hand should have length (2)
    game.dealerHand.hand.head.rank should equal (Three)
    game.dealerHand.hand.tail.head.rank should equal (Ace)
    Then("history should reflect that player has bet minimum bet")
    game.history.map(a => a.copy(bettingStrategy = None, minBetMultiplier = None)) should contain (Action(playerId = "player1", action = Bet, actionTokens = Some(game.minimumBet)))
    Then("history should show only 2 actions player has taken as Bet and IsDealt")
    game.history.filter(a => a.playerId == "player1") should have length (2)
    game.history.filter(a => a.playerId == "player1").map(_.action) should equal (Seq(Bet, IsDealt))
    Then("history should reflect that player has has been dealt 2 Twos")
    val dealtPlayerActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player1" && a.action == IsDealt).flatMap(_.actionCards)
    val dealtPlayerAfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player1" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtPlayerActionCardsFromHistory should have length (2)
    dealtPlayerAfterCardsFromHistory should have length (2)
    dealtPlayerActionCardsFromHistory.map(_.rank).head should equal (Two)
    dealtPlayerActionCardsFromHistory.map(_.rank).tail.head should equal (Two)
    Then("history should reflect that dealer has has been dealt a Three and an Ace")
    val dealtDealerActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId.toLowerCase() == "dealer" && a.action == IsDealt).flatMap(_.actionCards)
    val dealtDealerAfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId.toLowerCase() == "dealer" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtDealerActionCardsFromHistory should have length (2)
    dealtDealerAfterCardsFromHistory should have length (2)
    dealtDealerActionCardsFromHistory.map(_.rank).head should equal (Three)
    dealtDealerActionCardsFromHistory.map(_.rank).tail.head should equal (Ace)
  }

  it should "init a new 3-player game with player and dealer initial ranks to be overridden" in {
    Given("a BlackjackOptions which specifies player initial ranks override of [Two, Two] and dealer initial rank overrides of [Three, Ace]")
    val options = BlackjackOptions(
      deckCount = 6,
      dealerHitLimit = S17,
      blackjackPayout = ThreeToTwo,
      allowSurrender = true,
      hitOnSplitAces = true,
      resplitOnSplitAces = true,
      initialBank = 200, 
      playerInitialRanks = Seq(Two, Two), 
      dealerInitialRanks = Seq(Three, Ace)) 
    When("initializing a new 3-player game") 
    val game = module.init(3, options)
    Then("game players should be length 3 with player IDs 'player1', 'player2', 'player3'")
    game.players should have length (3)
    game.players.head.id should equal ("player1")
    game.players.tail.head.id should equal ("player2")
    game.players.tail.tail.head.id should equal ("player3")
    Then("each player's hand should reflect that it contains a pair of Twos")
    game.players should have length (3)
    game.players.head.hands should have length (1)
    game.players.head.hands.head should have length (2)
    game.players.head.hands.head.head.rank should equal (Two)
    game.players.head.hands.head.tail.head.rank should equal (Two)
    game.players.tail.head.hands should have length (1)
    game.players.tail.head.hands.head should have length (2)
    game.players.tail.head.hands.head.head.rank should equal (Two)
    game.players.tail.head.hands.head.tail.head.rank should equal (Two)
    game.players.tail.tail.head.hands should have length (1)
    game.players.tail.tail.head.hands.head should have length (2)
    game.players.tail.tail.head.hands.head.head.rank should equal (Two)
    game.players.tail.tail.head.hands.head.tail.head.rank should equal (Two)
    Then("dealer hand should reflect that it contains a Three and an Ace")
    game.dealerHand.hand should have length (2)
    game.dealerHand.hand.head.rank should equal (Three)
    game.dealerHand.hand.tail.head.rank should equal (Ace)
    Then("history should reflect that each player has bet minimum bet")
    game.history.map(a => a.copy(bettingStrategy = None, minBetMultiplier = None)) should contain (Action(playerId = "player1", action = Bet, actionTokens = Some(game.minimumBet)))
    game.history.map(a => a.copy(bettingStrategy = None, minBetMultiplier = None)) should contain (Action(playerId = "player2", action = Bet, actionTokens = Some(game.minimumBet)))
    game.history.map(a => a.copy(bettingStrategy = None, minBetMultiplier = None)) should contain (Action(playerId = "player3", action = Bet, actionTokens = Some(game.minimumBet)))
    Then("history should show only 2 actions player has taken as Bet and IsDealt")
    game.history.filter(a => a.playerId == "player1") should have length (2)
    game.history.filter(a => a.playerId == "player1").map(_.action) should equal (Seq(Bet, IsDealt))
    Then("history should reflect that each player has has been dealt 2 Twos")
    val dealtPlayer1ActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player1" && a.action == IsDealt).flatMap(_.actionCards)
    val dealtPlayer1AfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player1" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtPlayer1ActionCardsFromHistory should have length (2)
    dealtPlayer1AfterCardsFromHistory should have length (2)
    dealtPlayer1ActionCardsFromHistory.map(_.rank).head should equal (Two)
    dealtPlayer1ActionCardsFromHistory.map(_.rank).tail.head should equal (Two)
    val dealtPlayer2ActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player2" && a.action == IsDealt).flatMap(_.actionCards)
    val dealtPlayer2AfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player2" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtPlayer2ActionCardsFromHistory should have length (2)
    dealtPlayer2AfterCardsFromHistory should have length (2)
    dealtPlayer2ActionCardsFromHistory.map(_.rank).head should equal (Two)
    dealtPlayer2ActionCardsFromHistory.map(_.rank).tail.head should equal (Two)
    val dealtPlayer3ActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player3" && a.action == IsDealt).flatMap(_.actionCards)
    val dealtPlayer3AfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player3" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtPlayer3ActionCardsFromHistory should have length (2)
    dealtPlayer3AfterCardsFromHistory should have length (2)
    dealtPlayer3ActionCardsFromHistory.map(_.rank).head should equal (Two)
    dealtPlayer3ActionCardsFromHistory.map(_.rank).tail.head should equal (Two)
    Then("history should reflect that dealer has has been dealt a Three and an Ace")
    val dealtDealerActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId.toLowerCase() == "dealer" && a.action == IsDealt).flatMap(_.actionCards)
    val dealtDealerAfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId.toLowerCase() == "dealer" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtDealerActionCardsFromHistory should have length (2)
    dealtDealerAfterCardsFromHistory should have length (2)
    dealtDealerActionCardsFromHistory.map(_.rank).head should equal (Three)
    dealtDealerActionCardsFromHistory.map(_.rank).tail.head should equal (Ace)
  }

  it should "init a new 1-player game with player and dealer initial ranks to be overridden" in {
    Given("a BlackjackOptions which specifies player initial ranks override of [Ace, Ace] and dealer initial rank overrides of [Three, Four] as well as initial betting strategy Martingale")
    val options = BlackjackOptions(
      deckCount = 6,
      dealerHitLimit = S17,
      blackjackPayout = ThreeToTwo,
      allowSurrender = true,
      hitOnSplitAces = true,
      resplitOnSplitAces = true,
      initialBank = 200, 
      playerInitialRanks = Seq(Ace, Ace), 
      dealerInitialRanks = Seq(Three, Four),
      initialBettingStrategy = Some("Martingale")
    )
    When("initializing a new 1-player game") 
    val game = module.init(1, options)
    Then("game players should be length 1 with player IDs 'player1'")
    game.players should have length (1)
    game.players.head.id should equal ("player1")
    Then("player's hand should reflect that it contains a pair of Aces")
    game.players should have length (1)
    game.players.head.hands.head.head.rank should equal (Ace)
    game.players.head.hands.head.tail.head.rank should equal (Ace)
    Then("dealer hand should reflect that it contains a Three and an Ace")
    game.dealerHand.hand should have length (2)
    game.dealerHand.hand.head.rank should equal (Three)
    game.dealerHand.hand.tail.head.rank should equal (Four)
    Then("history should reflect that player has bet minimum bet")
    game.history should contain (Action(playerId = "player1", action = Bet, actionTokens = Some(game.minimumBet), bettingStrategy = Some("Martingale"), minBetMultiplier = Some(1.0))) 
    Then("history should show only 2 actions player has taken as Bet and IsDealt")
    game.history.filter(a => a.playerId == "player1") should have length (2)
    game.history.filter(a => a.playerId == "player1").map(_.action) should equal (Seq(Bet, IsDealt))
    Then("history should show all non-dealer player actions as having Martingale betting strategy")
    val playerHistory = game.history.filter(a => !a.playerId.toLowerCase().contains("dealer"))
    Then("history should reflect that player has has been dealt 2 Twos")
    var dealtPlayer1ActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player1" && a.action == IsDealt).flatMap(_.actionCards)
    var dealtPlayer1AfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId == "player1" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtPlayer1ActionCardsFromHistory should have length (2)
    dealtPlayer1AfterCardsFromHistory should have length (2)
    dealtPlayer1ActionCardsFromHistory.map(_.rank).head should equal (Ace)
    dealtPlayer1ActionCardsFromHistory.map(_.rank).tail.head should equal (Ace)
    Then("history should reflect that dealer has has been dealt a Three and an Ace")
    val dealtDealerActionCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId.toLowerCase() == "dealer" && a.action == IsDealt).flatMap(_.actionCards)
    val dealtDealerAfterCardsFromHistory: Seq[Card] = 
      game.history.filter(a => a.playerId.toLowerCase() == "dealer" && a.action == IsDealt).flatMap(_.afterCards).flatten
    dealtDealerActionCardsFromHistory should have length (2)
    dealtDealerAfterCardsFromHistory should have length (2)
    dealtDealerActionCardsFromHistory.map(_.rank).head should equal (Three)
    dealtDealerActionCardsFromHistory.map(_.rank).tail.head should equal (Four)
    // verify whether player Splits into 2 hands as expected
    val next = module.next(game)
    Then("history length: " + next.history.length)
    Then("history: " + next.history.mkString("\r\n"))
    Then("history should show player has Split")
    if (next.history.length == 4) {
      // length 4, player has split but hasn't yet next action beyond splitting 
      next.history.reverse.head.action should equal (Split)
      next.history.reverse.head.playerId.toLowerCase() should equal ("player1")
    } else {
      // length 5, player has both split and performed an additional action
      next.history.reverse.tail.head.playerId.toLowerCase() should equal ("player1")
      next.history.reverse.tail.head.action should equal (Split)
      next.history.reverse.head.playerId.toLowerCase() should equal ("player1")
    }
  }

  it should "not allow init when only player's initial ranks are overridden but not the dealer's" in {

    pending 
  }

  it should "not allow init when only dealer's initial ranks are overridden but not the player's" in {

    pending
  }

  it should "not allow init when player's and dealer's overridden initial hands have 3 cards" in {

    pending
  }

  it should "play a game to completion and verify that every Betting action belonging to a non-dealer player specifies betting strategy" in {
    Given("a BlackjackOptions which specifies initial-betting-strategy Martingale")
    val options = BlackjackOptions(
      initialBank = 2,
      initialBettingStrategy = Some("Martingale")
    )
    When("initializing a new 1-player game")
    val game = module.init(1, options)
    Then("new game state is initialized with an empty history")
    game.history shouldBe empty
    Then("game players should be length 1")
    game.players should have length (1)
    When("the game is played to completion")
    val results = module.completeGame(game)
    val dealerHistory = results.history.filter(_.playerId.toLowerCase() == "dealer")
    val playerHistory = results.history.diff(dealerHistory)
    // info("HISTORY NOT CONTAINING BETTING STRATEGY: " + playerHistory.filter(a => !a.bettingStrategy.isDefined).mkString("\r\n"))
    // info("HISTORY CONTAINING BETTING STRATEGY: " + playerHistory.filter(a => a.bettingStrategy.isDefined).mkString("\r\n"))
    Then("history shall reflect that all non-dealer players' Bet actions specify betting strategy used")
    // playerHistory.count(a => a.action == Bet && a.bettingStrategy.isDefined) should equal (playerHistory.count(a => a.action == Bet))
    playerHistory.count(a => a.action == Bet && a.bettingStrategy.isDefined) should equal (playerHistory.count(a => a.action == Bet))
    Then("history shall reflect that the dealer made no Bet actions")
    dealerHistory.count(a => a.action == Bet) should equal (0)
    Then("history shall reflect that all non-dealer players' Bet actions specify a min-bet multiplier used")
    playerHistory.count(a => a.action == Bet && a.minBetMultiplier.isDefined) should equal (playerHistory.count(a => a.action == Bet))
  }

}