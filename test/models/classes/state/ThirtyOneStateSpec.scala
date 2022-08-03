package cards.models.classes.state

import cards.models.classes.state.{ ThirtyOneGameState, ThirtyOnePlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class ThirtyOneStateSpec extends AnyFlatSpec {
  "ThirtyOneGameState" should "determine current player and next player" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    assert(initialState.currentPlayer() == players(0))
    assert(initialState.players(initialState.nextPlayerIndex()) == players(1))
    val nextState = initialState.copy(currentPlayerIndex = Some(initialState.nextPlayerIndex()))
    assert(nextState.currentPlayer() == players(1))
    assert(nextState.players(nextState.nextPlayerIndex()) == players(2))
    val nextNextState = nextState.copy(currentPlayerIndex = Some(nextState.nextPlayerIndex()))
    assert(nextNextState.currentPlayer() == players(2))
    assert(nextNextState.players(nextNextState.nextPlayerIndex()) == players(0))
    val lastState = nextNextState.copy(currentPlayerIndex = Some(nextNextState.nextPlayerIndex()))
    assert(lastState.currentPlayer() == players(0))
    assert(lastState.players(lastState.nextPlayerIndex()) == players(1))
  }

  it should "throw an illegal state exception getting current player when current player index isn't specified" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    try {
      val state = ThirtyOneGameState(players = players)
      state.currentPlayer()
      assert(false)
    } catch {
      case _: IllegalStateException => assert(true)
    }
  }
  
  it should "throw an illegal state exception getting next player index when current player index isn't specified" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    try {
      val state = ThirtyOneGameState(players = players)
      state.nextPlayerIndex()
      assert(false)
    } catch {
      case _: IllegalStateException => assert(true)
    }
  }

  it should "throw an illegal state exception getting current player when players list is empty" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    try {
      val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
      state.currentPlayer()
      assert(false)
    } catch {
      case _: IllegalStateException => assert(true)
    }
  }

  it should "throw an illegal state exception getting next player index when players list is empty" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    try {
      val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
      state.nextPlayerIndex()
      assert(false)
    } catch {
      case _: IllegalStateException => assert(true)
    }
  }

  it should "throw an illegal state exception getting current player when currentPlayerIndex is out of range" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    try {
      val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(3))
      state.currentPlayer()
      assert(false)
    } catch {
      case _: IllegalStateException => assert(true)
    }
  }

  it should "NOT throw an exception getting next player index when currentPlayerIndex is out of range, and instead use mod to reset back to the beginning" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    try {
      val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(3))
      val nextIndex: Int = state.nextPlayerIndex()
      assert(nextIndex == 1)
    } catch {
      case _: IllegalStateException => assert(false)
    }
  }

  it should "determine current and next player to be the same player when there is only a single player" in {
    val hands = Seq(
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    assert(initialState.currentPlayer() == players(0))
    assert(initialState.players(initialState.nextPlayerIndex()) == players(0))
    assert(initialState.players(initialState.nextPlayerIndex()) == initialState.currentPlayer())
    val nextState = initialState.copy(currentPlayerIndex = Some(initialState.nextPlayerIndex()))
    assert(nextState.currentPlayer() == players(0))
    assert(nextState.players(nextState.nextPlayerIndex()) == players(0))
    assert(nextState.players(nextState.nextPlayerIndex()) == nextState.currentPlayer())
  }

  it should "update hand with no changes to an empty player list and have it result in an empty list" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    val state = ThirtyOneGameState(players = players)
    val updatedPlayers = state.updatedHandAndSuspectedCards(Nil, Nil, Nil)
    assert(updatedPlayers == Nil)
  }
  
  it should "update hand with new cards to an empty player list and have it result in an empty list" in {
    val players: Seq[ThirtyOnePlayerState] = Nil
    val state = ThirtyOneGameState(players = players)
    val updatedPlayers = state.updatedHandAndSuspectedCards(Seq(Card(Seven, Diamonds), Card(Two, Hearts)), Nil, Nil)
    assert(updatedPlayers == Nil)
  }

  it should "update hand with discarded cards when discarded cards do NOT exist in the hand, resulting in no changes to the hand" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    assert(state.currentPlayer() == players(0))
    assert(state.currentPlayer().hand == hands(0))
    val updatedPlayers = state.updatedHandAndSuspectedCards(hands(0), Seq(Card(Ace, Spades)), Nil)
    assert(updatedPlayers == players)
  }

  it should "update hand and suspected cards when a new card is drawn from the discard pile" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0))
    assert(state.currentPlayer() == players(0))
    assert(state.currentPlayer().hand == hands(0))
    assert(state.currentPlayer().suspectedCards == Nil)
    val updatedPlayers = state.updatedHandAndSuspectedCards(hands(0) ++ Seq(Card(Ace, Spades)), Nil, Seq(Card(Ace, Spades)))
    assert(updatedPlayers != players)
    assert(updatedPlayers(0).hand == (hands(0) ++ Seq(Card(Ace, Spades))))
    assert(updatedPlayers(0).suspectedCards == Seq(Card(Ace, Spades)))
  }

}