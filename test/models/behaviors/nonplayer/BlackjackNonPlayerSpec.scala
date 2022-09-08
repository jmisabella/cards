package cards.models.behaviors.nonplayer

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.behaviors.betting.BlackjackBetting
import cards.models.behaviors.nonplayer.BlackjackNonPlayer
import cards.models.behaviors.play.BlackjackPlay
import cards.models.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.options.BlackjackPayout._
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
  private [nonplayer] case object _betting extends BlackjackBetting {
    override type EVAL = BlackjackHandEvaluation
    override val evaluation = _evaluation
  }
  private [nonplayer] case object _play extends BlackjackPlay

  case object module extends BlackjackNonPlayer {
    override type EVAL = BlackjackHandEvaluation
    override type PREDICATES = BlackjackPredicates
    override type BETTING = BlackjackBetting
    override type PLAY = BlackjackPlay
    override val evaluation = _evaluation
    override val predicates = _predicates
    override val betting = _betting
    override val play = _play
  }

  "BlackjackNonPlayer" should "throw an illegal state exception when proceeding to next state from a game state without any players" in {
    Given("a blackjack game state without any players")
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(), players = Nil)
    When("proceeding to the next state")
    Then("an illegal state exception should be thrown")
    an [IllegalStateException] shouldBe thrownBy (module.next(gameState)) 
  }

  "BlackjackNonPlayer" should "throw an illegal state exception when proceeding to next state from a game state with no designated current player" in {
    Given("a blackjack game state with 3 players but with no designated current player")
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
    val gameState = BlackjackGameState(options = BlackjackOptions(), dealerHand = Hand(dealerCards), players = Seq(player1, player2, player3), currentPlayerIndex = None)
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
        wins = Some(true))))
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
        wins = Some(true))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(payout = SixToFive), 
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
        wins = Some(true))))
    val dealerCards: Seq[Card] = Seq(Card(Ten, Diamonds), Card(Nine, Spades))
    val gameState = BlackjackGameState(
      options = BlackjackOptions(payout = OneToOne), 
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
        wins = Some(false))))
    val dealerCards: Hand = Hand(Seq(Card(Ace, Diamonds), Card(Ten, Spades)), Map("Jeffrey" -> 1), Some(true))
    val gameState = BlackjackGameState(
      dealerHand = dealerCards, 
      players = Seq(player1),
      currentPlayerIndex = Some(0))
    When("progressing to the next game state")
    val settledBets = module.next(gameState)  
    Then("player should win 2 for insurance but lose 1 for his losing hand, for a new bank total of 21")
    settledBets.players.head.bank should equal (21) 
  }

  // TODO: need to build logic so player will bet at a certain amount, and double bet after losing, and go back to original bet after winning
  // ......this will involve looking back in player's history

  it should "stand on a 3 card hand whose value is 20" in {
    
    pending
  }
}