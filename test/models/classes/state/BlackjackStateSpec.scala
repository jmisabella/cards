package cards.models.classes.state

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackStateSpec extends AnyFlatSpec with GivenWhenThen {

  "BlackjackState" should "throw unexpected state exception when calling current player when no players exist" in {
    Given("a blackjack game state with no players")
    val state: BlackjackGameState = BlackjackGameState(players = Nil, dealerHand = Hand())
    When("retrieving the current player")
    Then("an illegal state exception should be thrown")
    an [IllegalStateException] shouldBe thrownBy (state.currentPlayer())
  }

  it should "throw unexpected state exception when calling current hand when no players have any hands" in {
    Given("a blackjack game state 2 players, Patrick and Avery, who don't have any hands")
    val player1 = BlackjackPlayerState("Patrick Stewart", 20, Nil)
    val player2 = BlackjackPlayerState("Avery Brooks", 20, Nil)
    val state: BlackjackGameState = BlackjackGameState(players = Seq(player1, player2), dealerHand = Hand(), currentPlayerIndex = Some(0))
    When("retrieving the current player")
    val currentPlayer = state.currentPlayer()
    Then("Patrick should be retrieved") 
    currentPlayer should equal (player1)
    When("retrieving current player's current hand")
    Then("an illegal state exception should be thrown, since no players have any hands yet")
    an [IllegalStateException] shouldBe thrownBy (state.currentHand())
  }



}
