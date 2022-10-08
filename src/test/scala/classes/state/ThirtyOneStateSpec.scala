package cards.classes.state

import cards.classes.state.{ ThirtyOneGameState, ThirtyOnePlayerState }
import cards.classes.{ Card, Rank, Suit, Deck }
import cards.classes.Rank._
import cards.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ThirtyOneStateSpec extends AnyFlatSpec {
  "ThirtyOneGameState" should "determine current player and next player" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    initialState.currentPlayer() should equal (players(0))
    initialState.players(initialState.nextPlayerIndex()) should equal (players(1))
    val nextState = initialState.copy(currentPlayerIndex = Some(initialState.nextPlayerIndex()))
    nextState.currentPlayer() should equal (players(1))
    nextState.players(nextState.nextPlayerIndex()) should equal (players(2))
    val nextNextState = nextState.copy(currentPlayerIndex = Some(nextState.nextPlayerIndex()))
    nextNextState.currentPlayer() should equal (players(2))
    nextNextState.players(nextNextState.nextPlayerIndex()) should equal (players(0))
    val lastState = nextNextState.copy(currentPlayerIndex = Some(nextNextState.nextPlayerIndex()))
    lastState.currentPlayer() should equal (players(0))
    lastState.players(lastState.nextPlayerIndex()) should equal (players(1))
  }

  it should "throw an illegal state exception getting current player when current player index isn't specified" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players) 
    an [IllegalStateException] should be thrownBy state.currentPlayer()
  }
  
  it should "throw an illegal state exception getting next player index when current player index isn't specified" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players)
    an [IllegalStateException] should be thrownBy state.nextPlayerIndex() 
  }

  it should "throw an illegal state exception getting current player when players list is empty" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    an [IllegalStateException] should be thrownBy state.currentPlayer()
  }

  it should "throw an illegal state exception getting next player index when players list is empty" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    an [IllegalStateException] should be thrownBy state.nextPlayerIndex() 
  }

  it should "throw an illegal state exception getting current player when currentPlayerIndex is out of range" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(3))
    an [IllegalStateException] should be thrownBy state.currentPlayer()
  }

  it should "NOT throw an exception getting next player index when currentPlayerIndex is out of range, and instead use mod to reset back to the beginning" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(3))
    info("an illegal state exception should not be thrown by next player index") 
    val nextIndex: Int = state.nextPlayerIndex()
    nextIndex should equal (1)
  }

  it should "determine current and next player to be the same player when there is only a single player" in {
    val hands = Seq(
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    initialState.currentPlayer() should equal (players.head)
    initialState.players(initialState.nextPlayerIndex()) should equal (players(0))
    initialState.players(initialState.nextPlayerIndex()) should equal (initialState.currentPlayer())
    val nextState = initialState.copy(currentPlayerIndex = Some(initialState.nextPlayerIndex()))
    nextState.currentPlayer() should equal (players.head)
    nextState.players(nextState.nextPlayerIndex()) should equal (players.head)
    nextState.players(nextState.nextPlayerIndex()) should equal (nextState.currentPlayer())
  }

  it should "update hand with no changes to an empty player list and have it result in an empty list" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    val state = ThirtyOneGameState(players = players)
    val updatedPlayers = state.updatedHandAndSuspectedCards(Nil, Nil, false, Nil)
    updatedPlayers shouldBe empty
  }
  
  it should "update hand with new cards to an empty player list and have it result in an empty list" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    val state = ThirtyOneGameState(players = players)
    val updatedPlayers = state.updatedHandAndSuspectedCards(Seq(Card(Seven, Diamonds), Card(Two, Hearts)), Nil, false, Nil)
    updatedPlayers shouldBe empty
  }

  it should "update hand with discarded cards when discarded cards do NOT exist in the hand, resulting in no changes to the hand" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    state.currentPlayer() should equal (players(0))
    state.currentPlayer().hand should equal (hands(0))
    val updatedPlayers = state.updatedHandAndSuspectedCards(hands(0), Seq(Card(Ace, Spades)), false, Nil)
    updatedPlayers should equal (players)
  }

  it should "update hand and suspected cards when a new card is drawn from the discard pile" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    state.currentPlayer() should equal (players(0))
    state.currentPlayer().hand should equal (hands(0))
    state.currentPlayer().suspectedCards shouldBe empty
    val updatedPlayers = state.updatedHandAndSuspectedCards(hands(0) ++ Seq(Card(Ace, Spades)), Nil, false, Seq(Card(Ace, Spades)))
    updatedPlayers should not equal (players)
    updatedPlayers(0).hand should equal (hands(0) ++ Seq(Card(Ace, Spades)))
    updatedPlayers(0).suspectedCards should equal (Seq(Card(Ace, Spades)))
  }

  it should "not allow more than 7 players in a single game" in {
    val players = Seq(
      ThirtyOnePlayerState("player1", 3, Nil), 
      ThirtyOnePlayerState("player2", 3, Nil), 
      ThirtyOnePlayerState("player3", 3, Nil),
      ThirtyOnePlayerState("player4", 3, Nil),
      ThirtyOnePlayerState("player5", 3, Nil),
      ThirtyOnePlayerState("player6", 3, Nil),
      ThirtyOnePlayerState("player7", 3, Nil),
      ThirtyOnePlayerState("player8", 3, Nil))
      
    an [IllegalArgumentException] should be thrownBy ThirtyOneGameState(players = players) 
  }

  it should "allow 7 players in a single game" in {
    val players = Seq(
      ThirtyOnePlayerState("player1", 3, Nil), 
      ThirtyOnePlayerState("player2", 3, Nil), 
      ThirtyOnePlayerState("player3", 3, Nil),
      ThirtyOnePlayerState("player4", 3, Nil),
      ThirtyOnePlayerState("player5", 3, Nil),
      ThirtyOnePlayerState("player6", 3, Nil),
      ThirtyOnePlayerState("player7", 3, Nil))

    val state = ThirtyOneGameState(players = players) 
  }

}