package cards.behaviors.play

import cards.behaviors.play.BlackjackPlay
import cards.behaviors.Commons
import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.classes.{ Card, Rank, Suit, Deck, DeckType, Outcome }
import cards.classes.DeckType._
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.hand.Hand
import cards.classes.options.BlackjackOptions
import cards.classes.options.DealerHitLimit._
import cards.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.classes.options.BlackjackPayout._
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackPlaySpec extends AnyFlatSpec with GivenWhenThen {
  private case object _commons extends Commons
  private case object _evaluation extends BlackjackHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  private case object _play extends BlackjackPlay {
    override type COMMONS = Commons
    override val commons = _commons
    override type EVAL = BlackjackHandEvaluation
    override val evaluation: EVAL = _evaluation 
  }
  import _play._

  "BlackjackPlay" should "know when it's not yet time to play because bets have not yet been taken" in {
    Given("a game with 2 players but who have not yet placed any bets")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand()))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand()))
    val game = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Seq(player1, player2), currentPlayerIndex = Some(0))
    When("determining whether it's time to play game")
    val timeToPlay: Boolean = isTimeToPlay(game)
    Then("it's determined that it's not yet time to play because no bets have been taken by either player")
    timeToPlay shouldBe (false) 
  }

  it should "know when it's not yet time to play because only 1 out of 2 players have placed their bets" in {
    Given("a game with 2 players and only the first player has placed bets while 2nd player has not yet placed any bets")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Nil, bets = Map("Jeffrey" -> 10))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand()))
    val game = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Seq(player1, player2), currentPlayerIndex = Some(0))
    When("determining whether it's time to play game")
    val timeToPlay: Boolean = isTimeToPlay(game)
    Then("it's determined that it's not yet time to play because not all bets have been taken")
    timeToPlay shouldBe (false) 
  }
  
  it should 
  "know when it's time to deal because player's bet has been taken but player has not yet been dealt any cards" in {
    Given("a game with 1 player who has placed minimum bet but has not yet been dealt any cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Nil, bets = Map("Jeffrey" -> 5))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(hand = Nil, bets = Map("Alice" -> 5))))
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(), players = Seq(player1, player2), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal")
    val timeToPlay: Boolean = isTimeToDeal(game)
    Then("it's determined that it's indeed time to deal")
    timeToPlay shouldBe (true) 
  }
  
  it should 
  "know when it's time to play because bets have been taken and the player has 2 or more cards and hand is not yet flagged as either 'won' or 'lost'" in {
    Given("a game with 1 player who has 2 cards and his hand his not flagged as 'won' or 'lost'")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Two, Hearts), Card(Ten, Diamonds)), bets = Map("Jeffrey" -> 5), outcome = None)))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(hand = Seq(Card(Nine, Clubs), Card(Jack, Clubs)), bets = Map("Alice" -> 5), outcome = None)))
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(), players = Seq(player1, player2), currentPlayerIndex = Some(0))
    When("determining whether it's time to play game")
    val timeToPlay: Boolean = isTimeToPlay(game)
    Then("it's determined that it's indeed time to play")
    timeToPlay shouldBe (true) 
  }

  it should "throw an illegal argument exception when attempting to play but there are no players" in {
    Given("a game with no players")
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(), players = Nil)
    When("determining whether it's time to play game")
    Then("an illegal state exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (isTimeToPlay(game))
    When("attempting to play")
    Then("an illegal state exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (playHand(game))
  }

  it should "throw an illegal argument exception when attempting to play but current player is not specified" in {
    Given("a game with 2 players who each have 2 cards and whose hand his not flagged as 'won' or 'lost', and current player is not specified")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Two, Hearts), Card(Ten, Diamonds)), bets = Map("Jeffrey" -> 5), outcome = None)))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(hand = Seq(Card(Nine, Clubs), Card(Jack, Clubs)), bets = Map("Alice" -> 5), outcome = None)))
    // current player is not specified
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(), players = Seq(player1, player2), currentPlayerIndex = None)
    When("attempting to play")
    Then("an illegal state exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (playHand(game))
  }

  it should "throw an illegal argument exception when attempting to play but current player's hand is of length 1" in {
    Given("a game with 1 player who has only 1 card his not flagged as 'won' or 'lost'")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Two, Hearts)), bets = Map("Jeffrey" -> 5), outcome = None)))
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Three, Hearts), Card(Two, Hearts))), players = Seq(player1), currentPlayerIndex = Some(0))
    When("attempting to play")
    Then("an illegal state exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (playHand(game))
  }

  it should "throw an illegal argument exception when attempting to player for a single player with 2 cards but dealer only has 1 card" in {
    Given("a game with 1 player who has 2 cards and his not flagged as 'won' or 'lost' and a dealer who has only 1 card")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25,
      Seq( 
        Hand(hand = Seq(Card(Two, Hearts), Card(Ten, Diamonds)), bets = Map("Jeffrey" -> 5), outcome = None)))
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Ace, Spades))), players = Seq(player1), currentPlayerIndex = Some(0))
    When("attempting to play")
    Then("an illegal state exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (playHand(game))
  }

  it should "not allow split for an empty hand" in {
    val cards: Seq[Card] = Nil
    val result: Boolean = canSplit(cards)
    result should be (false)
  }

  it should "not allow split for a single-card hand" in {
    val cards: Seq[Card] = Seq(Card(Three, Clubs))
    val result: Boolean = canSplit(cards)
    result should be (false)
  }

  it should "not allow split for a 2-card hand with cards of different rank whose values are both lower than 10" in {
    val cards: Seq[Card] = Seq(Card(Seven, Hearts), Card(Nine, Spades))
    val result: Boolean = canSplit(cards)
    result should be (false)
  }

  it should "allow split for a 2-card hand with cards of matching rank" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Hearts))
    val result: Boolean = canSplit(cards)
    result should be (true)
  }

  it should "by default allow split for a 2-card hand consisting of a Ten and a Queen" in {
    val cards: Seq[Card] = Seq(Card(Ten, Clubs), Card(Queen, Hearts))
    val result: Boolean = canSplit(cards)
    result should be (true)
  }
  
  it should "allow split for a 2-card hand consisting of a Ten and a Queen (differ in rank, but share same value 10)" in {
    val cards: Seq[Card] = Seq(Card(Ten, Clubs), Card(Queen, Hearts))
    val result: Boolean = canSplit(cards)
    result should be (true)
  }
  
  it should "allow split for a 2-card hand consisting of two Tens" in {
    val cards: Seq[Card] = Seq(Card(Ten, Clubs), Card(Ten, Hearts))
    val result: Boolean = canSplit(cards)
    result should be (true)
  }

  it should "not allow split for a 3-card hand with 2 cards of matching rank and one additional card" in {
    val cards: Seq[Card] = Seq(Card(Jack, Clubs), Card(Jack, Hearts), Card(Two, Clubs))
    val result: Boolean = canSplit(cards)
    result should be (false)
  }

  it should "allow split for pair of sevens if split limit of 2 has not yet been reached (split count is 1)" in {
    val cards: Seq[Card] = Seq(Card(Seven, Clubs), Card(Seven, Hearts))
    val result: Boolean = canSplit(cards, BlackjackOptions(splitLimit = Some(2)), splitCount = 1)
    result should be (true)
  }
  
  it should "not allow split for pair of threes if split limit of 2 has been reached (split count is 2)" in {
    val cards: Seq[Card] = Seq(Card(Three, Clubs), Card(Three, Hearts))
    val result: Boolean = canSplit(cards, BlackjackOptions(splitLimit = Some(2)), splitCount = 2)
    result should be (false)
  }

  it should "not allow split for pair of threes if split limit of 2 has been exceeded (split count is 3)" in {
    val cards: Seq[Card] = Seq(Card(Three, Clubs), Card(Three, Hearts))
    val result: Boolean = canSplit(cards, BlackjackOptions(splitLimit = Some(2)), splitCount = 3)
    result should be (false)
  }

  it should ", when resplitOnSplitAces is false, allow split for pair of aces if there were no previous aces split during this turn" in {
    val cards: Seq[Card] = Seq(Card(Ace, Clubs), Card(Ace, Hearts))
    val result: Boolean = canSplit(cards, BlackjackOptions(splitLimit = Some(2), resplitOnSplitAces = false), splitCount = 0, splitAcesCount = 0)
    result should be (true)
  }

  it should ", when resplitOnSplitAces is false, not allow split for pair of aces if there were already aces split during this turn" in {
    val cards: Seq[Card] = Seq(Card(Ace, Clubs), Card(Ace, Hearts))
    val result: Boolean = canSplit(cards, BlackjackOptions(splitLimit = Some(2), resplitOnSplitAces = false), splitCount = 0, splitAcesCount = 1)
    result should be (false)
  }

  it should "always split player's aces for first time regardless dealer's face up card" in {
    Given("a player with pair of aces and has not yet split any cards and who has bet minimum bet on his hand")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Ace, Hearts), Card(Ace, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)))
    Given("dealer hand of 2 cards and two is face up card")
    var dealer = Hand(Seq(Card(Two, Hearts), Card(Two, Clubs)))
    var game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    var result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
    Given("dealer 2-card hand with face up card three")
    dealer = Hand(Seq(Card(Three, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  

    Given("dealer 2-card hand with face up card Four")
    dealer = Hand(Seq(Card(Four, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  

    Given("dealer 2-card hand with face up card Five")
    dealer = Hand(Seq(Card(Five, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
  
    Given("dealer 2-card hand with face up card Six")
    dealer = Hand(Seq(Card(Six, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
  
    Given("dealer 2-card hand with face up card Seven")
    dealer = Hand(Seq(Card(Seven, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
  
    Given("dealer 2-card hand with face up card Eight")
    dealer = Hand(Seq(Card(Eight, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
  
    Given("dealer 2-card hand with face up card Nine")
    dealer = Hand(Seq(Card(Nine, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
  
    Given("dealer 2-card hand with face up card Ten")
    dealer = Hand(Seq(Card(Ten, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
  
    Given("dealer 2-card hand with face up card Ace")
    dealer = Hand(Seq(Card(Ace, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should split")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Split && a.actionTokens == 5) >= 1)  
  }

  it should "always stand on player's 2-card hand of two cards valued at 10 each (score of 20) and never split, regardless dealer's showing card" in {
    Given("a player with pair of 10s and has not yet split any cards and who has bet minimum bet on his hand")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Queen, Hearts), Card(King, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)))
    Given("dealer hand of 2 cards and Two is face up card")
    var dealer = Hand(Seq(Card(Two, Hearts), Card(Two, Clubs)))
    var game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    var result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Three is face up card")
    dealer = Hand(Seq(Card(Three, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Four is face up card")
    dealer = Hand(Seq(Card(Four, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Five is face up card")
    dealer = Hand(Seq(Card(Five, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Six is face up card")
    dealer = Hand(Seq(Card(Six, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Seven is face up card")
    dealer = Hand(Seq(Card(Seven, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Eight is face up card")
    dealer = Hand(Seq(Card(Eight, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Nine is face up card")
    dealer = Hand(Seq(Card(Nine, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Jack is face up card")
    dealer = Hand(Seq(Card(Jack, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  

    Given("dealer hand of 2 cards and Ace is face up card")
    dealer = Hand(Seq(Card(Ace, Hearts), Card(Two, Clubs)))
    game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = dealer, 
      players = Seq(player1), 
      currentPlayerIndex = Some(0), 
      currentHandIndex = Some(0))
    When("playing player's hand")
    result = playHand(game)
    Then("player should Stand")
    assert(result.history.count(a => a.playerId == "Jeffrey" && a.action == Stand && a.actionTokens == 0 && a.actionCards == Nil) >= 1)  
  }

  it should "know it's time to deal when bets have been accepted for a 1 player game but neither player nor dealer have any cards" in {
    Given("a game with 1 player who has bet but has no cards, and a dealer who also has no cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Nil, bets = Map("Jeffrey" -> 5), outcome = None)))
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(), players = Seq(player1), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined that yes, it is in fact time to deal")
    result shouldBe (true)
  }

  it should "know it's time to deal when bets have been accepted for a 1 player game and player has cards but dealer does not" in {
    Given("a game with 1 player who has two cards and a dealer who has no cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Three, Hearts), Card(Seven, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)))
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(), players = Seq(player1), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined that yes, it is in fact time to deal")
    result shouldBe (true)
    When("dealing cards")
    val cardsDealtState = deal(game) 
    Then("0 cards should be dealt to player and 2 cards should be dealt to the dealer, resulting in both player and dealer having exactly 2 cards")
    val allHands: Seq[Seq[Card]] = cardsDealtState.players.map(_.hands).flatten ++ Seq(cardsDealtState.dealerHand.hand)
    allHands should have length (2)
    info("player should have 2 cards")
    allHands.head should have length (2)
    info("dealer should have 2 cards")
    allHands.tail.head should have length (2)
  }

  it should "know it's time to deal when bets have been accepted for a 1 player game and player has no cards but dealer has 2 cards" in {
    Given("a game with 1 player who has no cards and a dealer who has two cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Nil, bets = Map("Jeffrey" -> 5), outcome = None)))
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Three, Hearts), Card(Seven, Clubs))), players = Seq(player1), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined that yes, it is in fact time to deal")
    result shouldBe (true)
    When("dealing cards")
    val cardsDealtState = deal(game) 
    Then("2 cards should be dealt to player and no cards should be dealt to the dealer, resulting in both player and dealer having exactly 2 cards")
    val allHands: Seq[Seq[Card]] = cardsDealtState.players.map(_.hands).flatten ++ Seq(cardsDealtState.dealerHand.hand)
    allHands should have length (2)
    info("player should have 2 cards")
    allHands.head should have length (2)
  }

  it should "know it's time to deal when bets have been accepted for a 1 player game and player has 2 cards but dealer has only 1 cards" in {
    Given("a game with 1 player who has two cards and a dealer who has only one card")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Three, Hearts), Card(Seven, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Four, Clubs))), players = Seq(player1), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined that yes, it is in fact time to deal")
    result shouldBe (true)
    When("dealing cards")
    val cardsDealtState = deal(game) 
    Then("0 cards should be dealt to player and 1 card should be dealt to the dealer, resulting in both player and dealer having exactly 2 cards")
    val allHands: Seq[Seq[Card]] = cardsDealtState.players.map(_.hands).flatten ++ Seq(cardsDealtState.dealerHand.hand)
    allHands should have length (2)
    info("player should have 2 cards")
    allHands.head should have length (2)
    info("dealer should have 2 cards")
    allHands.tail.head should have length (2)
    info("original game's history is empty")
    game.history shouldBe empty
    info("updated history should have length 1")
    cardsDealtState.history should have length (1)
  }

  it should "know it's not time to deal when bets have been accepted for a 1 player game and player has 3 cards and the dealer 2" in {
    Given("a game with 1 player who has three cards and a dealer who has two cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Three, Hearts), Card(Seven, Clubs), Card(Ace, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Four, Clubs), Card(Ten, Clubs))), players = Seq(player1), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined that it's not time to deal")
    result shouldBe (false)
    When("attempting to deal")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (deal(game))
  }

  it should "know it's not time to deal in a game with no players" in {
    Given("a game with no players")
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Four, Clubs), Card(Ten, Clubs))), players = Nil, currentPlayerIndex = None)
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined it's not time to deal")
    result shouldBe false
    When("attempting to deal")
    Then("an illegal argument exception should be thrown")
    an [IllegalArgumentException] shouldBe thrownBy (deal(game))
  }

  it should "know it's time to deal when bets have been accepted for a 1 player game and player has 3 cards and the dealer only has 1 card" in {
    Given("a game with 1 player who has three cards and a dealer who has only one card")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Three, Hearts), Card(Seven, Clubs), Card(Ace, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Four, Clubs))), players = Seq(player1), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined that yes, it is in fact time to deal")
    result shouldBe (true)
  }

  it should "know it's time to deal when bets have been accepted for a 2 player game with 1 player having 2 cards and the other player having no cards" in {
    Given("a game with 2 players, one having 2 cards and another with no cards, and a dealer with 2 cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Three, Hearts), Card(Seven, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val player2 = BlackjackPlayerState(
      "Brandon", 
      25, 
      Seq( 
        Hand(hand = Nil, bets = Map("Brandon" -> 5), outcome = None)) )
    val game = BlackjackGameState(options = BlackjackOptions(), minimumBet = 5, dealerHand = Hand(hand = Seq(Card(Four, Clubs), Card(Nine, Clubs))), players = Seq(player1, player2), currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    val result = isTimeToDeal(game)
    Then("it's determined that yes, it is in fact time to deal")
    result shouldBe (true)
  }

  it should "know it's time for dealer to play when only player has selected to Stand" in {
    Given("a game with 1 player having 2 cards, a dealer with 2 cards, and a history showing the player is Standing")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Ten, Hearts), Card(Eight, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val history = Seq(Action("Jeffrey", Stand, Nil, 0, Seq(Card(Ten, Hearts), Card(Eight, Clubs)), Seq(Seq(Card(Ten, Hearts), Card(Eight, Clubs)))))
    val game = BlackjackGameState(
      minimumBet = 5, 
      dealerHand = Hand(hand = Seq(Card(Nine, Diamonds), Card(Nine, Clubs))), 
      players = Seq(player1), 
      history = history, 
      currentPlayerIndex = None)
    When("determining whether it's time to deal cards")
    Then("it's determined that it's not time to deal")
    isTimeToDeal(game) shouldBe (false)
    When("determining whether it's time for dealer to play")
    Then("it's determined that yes, it is in fact time for dealer to play")
    isTimeForDealerToPlay(game) shouldBe (true)
  }

  it should "know it's time for dealer to play when only player has busted" in {
    Given("a game with 1 player having 3 cards and whose hand is busted, a dealer with 2 cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Ten, Hearts), Card(Eight, Clubs), Card(Four, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val history = Seq(Action("Jeffrey", Hit, Seq(Card(Four, Clubs)), 0, Seq(Card(Ten, Hearts), Card(Eight, Clubs)), Seq(Seq(Card(Ten, Hearts), Card(Eight, Clubs), Card(Four, Clubs)))))
    val game = BlackjackGameState(
      options = BlackjackOptions(dealerHitLimit = H17), 
      minimumBet = 5, 
      // dealer has soft 17
      dealerHand = Hand(hand = Seq(Card(Six, Clubs), Card(Ace, Clubs))), 
      players = Seq(player1), 
      history = history, 
      currentPlayerIndex = None)
    When("determining whether it's time to deal cards")
    Then("it's determined that it's not time to deal")
    isTimeToDeal(game) shouldBe (false)
    When("determining whether it's time for dealer to play")
    Then("it's determined that yes, it is in fact time for dealer to play")
    isTimeForDealerToPlay(game) shouldBe (true)
  }

  it should "know it's time for dealer to play when 2 players have played (one busts, another Stands)" in {
    Given("a game with 2 players, one who busts and another who Stands, and a dealer with 2 cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Ten, Hearts), Card(Seven, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val player2 = BlackjackPlayerState(
      "Brandon", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Five, Diamonds), Card(Nine, Hearts), Card(Eight, Diamonds)), bets = Map("Brandon" -> 5), outcome = None)) )
    val history = Seq(
      Action("Brandon", Hit, Seq(Card(Eight, Diamonds)), 0, Seq(Card(Five, Diamonds), Card(Nine, Hearts)), Seq(Seq(Card(Five, Diamonds), Card(Nine, Hearts), Card(Eight, Diamonds)))),
      Action("Jeffrey", Stand, Nil, 0, Seq(Card(Ten, Hearts), Card(Seven, Clubs)), Seq(Seq(Card(Ten, Hearts), Card(Seven, Clubs)))) )
    val game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      // dealer has soft 17
      dealerHand = Hand(hand = Seq(Card(Six, Clubs), Card(Ace, Clubs))), 
      players = Seq(player1, player2), 
      history = history, 
      currentPlayerIndex = None)
    When("determining whether it's time to deal cards")
    Then("it's determined that it's not time to deal")
    isTimeToDeal(game) shouldBe (false)
    When("determining whether it's time for dealer to play")
    Then("it's determined that yes, it is in fact time for dealer to play")
    isTimeForDealerToPlay(game) shouldBe (true)
    When("when dealer plays hand on an H17 board")
    val playedHand = dealerPlay(game.copy(options = game.options.copy(dealerHitLimit = H17))) 
    Then("then dealer will hit on soft 17")
    val action = playedHand.history.reverse.head
    action.playerId should equal ("Dealer")
    action.action should equal (Hit)
  }

  it should "know it's not time for dealer to play when only 1 of 2 players has finished playing (busted)" in {
    Given("a game with 2 players, one who busts and another hasn't busted nor Standed, and a dealer with 2 cards")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Ten, Hearts), Card(Seven, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val player2 = BlackjackPlayerState(
      "Brandon", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Five, Diamonds), Card(Nine, Hearts), Card(Eight, Diamonds)), bets = Map("Brandon" -> 5), outcome = None)) )
    val history = Seq(
      Action("Brandon", Hit, Seq(Card(Eight, Diamonds)), 0, Seq(Card(Five, Diamonds), Card(Nine, Hearts)), Seq(Seq(Card(Five, Diamonds), Card(Nine, Hearts), Card(Eight, Diamonds)))))
    val game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = Hand(hand = Seq(Card(Four, Clubs), Card(Nine, Clubs))), 
      players = Seq(player1, player2), 
      history = history, 
      currentPlayerIndex = Some(0))
    When("determining whether it's time to deal cards")
    Then("it's determined that it's not time to deal")
    isTimeToDeal(game) shouldBe (false)
    When("determining whether it's time for dealer to play")
    Then("it's determined that it's not time for dealer to play")
    isTimeForDealerToPlay(game) shouldBe (false)
  }

  it should "know it's not time for dealer to play when 2 players have played (one busts, another Stands) but dealer only has 1 card" in {
    Given("a game with 2 players, one who busts and another who Stands, and a dealer with only 1 card")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Ten, Hearts), Card(Seven, Clubs)), bets = Map("Jeffrey" -> 5), outcome = None)) )
    val player2 = BlackjackPlayerState(
      "Brandon", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Five, Diamonds), Card(Nine, Hearts), Card(Eight, Diamonds)), bets = Map("Brandon" -> 5), outcome = None)) )
    val history = Seq(
      Action("Brandon", Hit, Seq(Card(Eight, Diamonds)), 0, Seq(Card(Five, Diamonds), Card(Nine, Hearts)), Seq(Seq(Card(Five, Diamonds), Card(Nine, Hearts), Card(Eight, Diamonds)))),
      Action("Jeffrey", Stand, Nil, 0, Seq(Card(Ten, Hearts), Card(Seven, Clubs)), Seq(Seq(Card(Ten, Hearts), Card(Seven, Clubs)))) )
    val game = BlackjackGameState(
      options = BlackjackOptions(), 
      minimumBet = 5, 
      dealerHand = Hand(hand = Seq(Card(Nine, Clubs))), 
      players = Seq(player1, player2), 
      history = history, 
      currentPlayerIndex = Some(1))
    When("determining whether it's time to deal cards")
    Then("it's determined that it's time to deal")
    isTimeToDeal(game) shouldBe (true)
    When("determining whether it's time for dealer to play")
    Then("it's determined that it's not time for dealer to play")
    isTimeForDealerToPlay(game) shouldBe (false)
  }

}
