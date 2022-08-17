package cards.models.classes.state

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
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
}