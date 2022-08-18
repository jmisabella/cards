package cards.models.classes.state

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackOptions
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackStateSpec extends AnyFlatSpec with GivenWhenThen {
  "BlackjackPlayerState" should "yield all bets a given player has placed on a specific Blackjack player's hand" in {
    Given("a player state who has placed bet on his own hand and who also has 2 other players who have placed bets on his hand")
    val player = BlackjackPlayerState("Jeffrey", 50, Seq( (Seq(Card(Ten, Clubs), Card(Jack, Hearts)), Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15))))

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
        (Seq(Card(Eight, Hearts), Card(Jack, Diamonds)), 
        Map("Jeffrey" -> 15, "Alice" -> 10))))
    val player2 = BlackjackPlayerState(
      "Alice", 
      50, 
      Seq( 
        (Seq(Card(Ten, Clubs), Card(Ace, Spades)), 
        Map("Jeffrey" -> 5, "Brandon" -> 10, "Alice" -> 15))))
    val player3 = BlackjackPlayerState(
      "Brandon", 
      40, 
      Seq( 
        (Seq(Card(Ten, Spades), Card(Seven, Hearts), Card(Ace, Clubs)), 
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
}