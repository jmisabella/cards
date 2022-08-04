package cards.models.behaviors.nonplayer

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.models.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.state.{ ThirtyOneGameState, ThirtyOnePlayerState }
import cards.models.classes.actions.{ Action, ThirtyOneAction }
import cards.models.classes.actions.ThirtyOneAction._
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

  it should "knock if it's the very first turn and current player's score is greater than 17" in {
    val hands = Seq(
      Seq(Card(Three, Clubs), Card(Five, Clubs), Card(Jack, Clubs)),  // 18, greater than 17
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val discardPile = Seq(Card(Nine, Clubs))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile)
    assert(!initialState.knockedPlayerId.isDefined) 
    val nextState = module.next(initialState)
    assert(nextState.knockedPlayerId.isDefined) 
    assert(nextState.knockedPlayerId.get == players.head.id) // player1 knocked
  }

  it should "knock if it's first round and current player's score is greater than 21 and next player cannot pick up 31 from discard pile" in {
    val hands = Seq(
      Seq(Card(Three, Clubs), Card(Nine, Clubs), Card(Jack, Clubs)),  // current player 23, greater than 21
      Seq(Card(Ten, Hearts), Card(Ace, Hearts), Card(Seven, Spades)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val discardPile = Seq(Card(Eight, Clubs)) // next player would not get 31 if picking this up
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = Seq(Action("player3", Draw, Seq(Card(Four, Diamonds))), Action("player3", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(!initialState.knockedPlayerId.isDefined) 
    val nextState = module.next(initialState)
    assert(nextState.knockedPlayerId.isDefined) 
    assert(nextState.knockedPlayerId.get == players.head.id) // player1 knocked
  }

  it should "not knock if it's first round and current player's score is greater than 21 and next player can pick up 31 from discard pile" in {
    val hands = Seq(
      Seq(Card(Ten, Hearts), Card(Ace, Hearts), Card(Seven, Spades)), // next player would get 31 if picking up the Jack of hearts from discard pile 
      Seq(Card(Three, Clubs), Card(Nine, Clubs), Card(Jack, Clubs)),
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades))) // current player, 27, greater than 21
    val discardPile = Seq(Card(Jack, Hearts)) // next player would get 31 if picking this up
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0), suspectedCards = Seq(Card(Ten, Hearts), Card(Ace, Hearts), Card(Seven, Spades))),
        ThirtyOnePlayerState("player2", 3, hands(1)), 
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = Seq(Action("player3", Draw, Seq(Card(Four, Diamonds))), Action("player3", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(2), discardPile = discardPile, history = history)
    assert(!initialState.knockedPlayerId.isDefined)
    assert(initialState.nextPlayerIndex() == 0) // next player is player1
    val nextState = module.next(initialState)
    assert(!nextState.knockedPlayerId.isDefined) 
  }

  it should "draw from discard pile if it leads to current player getting a 31 (instant win), even if discarded card leads to next player potentially getting 31" in {
    val hands = Seq(
      Seq(Card(Ten, Hearts), Card(Ace, Hearts), Card(Jack, Clubs)), // current player, would get 31 if drawing from discard pile
      Seq(Card(Queen, Clubs), Card(Ace, Clubs), Card(Seven, Spades)), // if next player picks up discarded jack of clubs, would also get thirty one
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades))) 
    val discardPile = Seq(Card(Jack, Hearts)) // if current player picks up, would get 31, however he would discard a card which would lead to next player's suspected being 31 
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1), suspectedCards = Seq(Card(Queen, Clubs), Card(Ace, Clubs), Card(Seven, Spades))),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = 
      Seq(
        Action("player1", Draw, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", Draw, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", Draw, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", Draw, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(!initialState.knockedPlayerId.isDefined)
    assert(initialState.nextPlayerIndex() == 1) // next player is player 2
    val nextState = module.next(initialState)
    assert(!nextState.knockedPlayerId.isDefined) 
    assert(nextState.history.length >= 2)
    assert(nextState.history.reverse.head == Action("player1", Discard, Seq(Card(Jack, Clubs)))) // shows after current player had drawn from discard, she'd discarded the clubs card
    assert(nextState.history.reverse.tail.head == Action("player1", DrawFromDiscard, Seq(Card(Jack, Hearts)))) // shows current player had drawn from the discard pile
  }

}
