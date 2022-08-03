package cards.models.behaviors.nonplayer

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.models.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.state.{ ThirtyOneGameState, ThirtyOnePlayerState }
import org.scalatest.flatspec.AnyFlatSpec

class ThirtyOneNonPlayerSpec extends AnyFlatSpec {
  private[nonplayer] case object _commons extends Commons
  private [nonplayer] case object _evaluation extends ThirtyOneHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  case object module extends ThirtyOneNonPlayer {
    override type EVAL = ThirtyOneHandEvaluation
    override val evaluation = _evaluation
  }

  "ThirtyOneNonPlayer" should "throw IllegalStateException when calling next() from game state which does not yet have any player states" in {
    try {
      val gameState = ThirtyOneGameState(Nil)
      val result = module.next(gameState) 
      assert(false)
    } catch {
      case e: IllegalStateException => assert(true)
    }
  }

  it should "throw illegal state exception when calling next() from game state which has an empty deck" in {
    try {
      val hands = Seq(
        Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
        Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
        Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
      val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
      val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), deck = Deck.emptyDeck) 
      val nextState = module.next(state)
      assert(false)
    } catch {
      case e: IllegalStateException => assert(true)
    }
  }

  it should "throw illegal state exception when calling next() from game state which has an empty discard pile" in {
    try {
      val hands = Seq(
        Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
        Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
        Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
      val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
      val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = Nil) 
      val nextState = module.next(state)
      assert(false)
    } catch {
      case e: IllegalStateException => assert(true)
    }
  }

  it should "throw illegal state exception when calling next() from game state which already has a declared winner" in {
    try {
      val hands = Seq(
        Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
        Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
        Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
      val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
      val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), winningPlayerId = Some(players.reverse.head.id))
      val nextState = module.next(state)
      assert(false)
    } catch {
      case e: IllegalStateException => assert(true)
    }
  }

  it should "declare a winner who is not the current player has 31" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Ace, Spades), Card(Ten, Spades), Card(Queen, Spades)))
    val discardPile = Seq(Card(Jack, Clubs))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile)
    val nextState = module.next(initialState)
    assert(nextState.players == initialState.players) // no change in players
    assert(nextState.winningPlayerId.isDefined)
    assert(nextState.winningPlayerId.get == players.reverse.head.id) // player3 was the winner with 31, was not the current player
  }

  it should "declare a winner who is the current player has 31" in {
    val hands = Seq(
      Seq(Card(Ace, Spades), Card(Ten, Spades), Card(Queen, Spades)),
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)))
    val discardPile = Seq(Card(Jack, Clubs))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile)
    val nextState = module.next(initialState)
    assert(nextState.players == initialState.players) // no change in players
    assert(nextState.winningPlayerId.isDefined)
    assert(nextState.winningPlayerId.get == players.head.id) // player1 (the current player) was the winner with 31
  }

  it should "declare a winner when knocked player is the current player" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val discardPile = Seq(Card(Jack, Clubs))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, knockedPlayerId = Some(players.head.id))
    assert(!initialState.winningPlayerId.isDefined) 
    val nextState = module.next(initialState)
    assert(nextState.winningPlayerId.isDefined) 
    assert(nextState.winningPlayerId.get == players.reverse.head.id) // player3 was the winner 
  }

}
