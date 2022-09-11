package cards.models.behaviors.play

import cards.models.behaviors.play.BlackjackPlay
import cards.models.behaviors.Commons
import cards.models.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackPlaySpec extends AnyFlatSpec with GivenWhenThen {
  private case object _commons extends Commons
  private case object _play extends BlackjackPlay {
    override type C = Commons
    override val commons = _commons
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
  "know when it's time to play because player's bet has been taken but player has not yet been dealt any cards" in {
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
    When("determining whether it's time to play game")
    val timeToPlay: Boolean = isTimeToPlay(game)
    Then("it's determined that it's indeed time to play")
    timeToPlay shouldBe (true) 
  }
  
  it should 
  "know when it's time to play because bets have been taken and the player has 2 or more cards and hand is not yet flagged as either 'won' or 'lost'" in {
    Given("a game with 1 player who has 2 cards and his hand his not flagged as 'won' or 'lost'")
    val player1 = BlackjackPlayerState(
      "Jeffrey", 
      25, 
      Seq( 
        Hand(hand = Seq(Card(Two, Hearts), Card(Ten, Diamonds)), bets = Map("Jeffrey" -> 5), wins = None)))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        Hand(hand = Seq(Card(Nine, Clubs), Card(Jack, Clubs)), bets = Map("Alice" -> 5), wins = None)))
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

}
 