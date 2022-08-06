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
import org.scalatest.matchers.should.Matchers._

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
    val gameState = ThirtyOneGameState(Nil)
    an [IllegalStateException] should be thrownBy module.next(gameState) 
  }

  it should "throw illegal state exception when calling next() from game state which has an empty deck" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), deck = Deck.emptyDeck) 
    an [IllegalStateException] should be thrownBy module.next(state)
  }

  it should "throw illegal state exception when calling next() from game state which has an empty discard pile" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = Nil) 
    an [IllegalStateException] should be thrownBy module.next(state)
  }

  it should "throw illegal state exception when calling next() from game state which already has a declared winner" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val state = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), winningPlayerId = Some(players.reverse.head.id))
    an [IllegalStateException] should be thrownBy module.next(state)
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
    info("no change in players")
    nextState.players should equal (initialState.players)
    nextState.winningPlayerId shouldBe defined
    info("player3 was the winner with 31, was not the current player")
    nextState.winningPlayerId.get should equal (players.reverse.head.id)
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
    info("no change in players")
    nextState.players should equal (initialState.players)
    nextState.winningPlayerId shouldBe defined
    info("player1 (the current player) was the winner with 31")
    nextState.winningPlayerId.get should equal (players.head.id)
  }

  it should "declare a winner when knocked player is the current player" in {
    val hands = Seq(
      Seq(Card(Two, Clubs), Card(Three, Clubs), Card(Four, Clubs)), 
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val discardPile = Seq(Card(Jack, Clubs))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, knockedPlayerId = Some(players.head.id))
    initialState.winningPlayerId should not be defined
    initialState.winningPlayerId shouldBe empty
    val nextState = module.next(initialState)
    nextState.winningPlayerId shouldBe defined
    info("player3 was the winner")
    nextState.winningPlayerId.get should equal (players.reverse.head.id)
  }

  it should "knock if it's the very first turn and current player's score is greater than 17" in {
    val hands = Seq(
      Seq(Card(Three, Clubs), Card(Five, Clubs), Card(Jack, Clubs)),  // 18, greater than 17
      Seq(Card(Five, Hearts), Card(Six, Hearts), Card(Seven, Hearts)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val discardPile = Seq(Card(Nine, Clubs))
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile)
    initialState.knockedPlayerId should not be defined
    initialState.knockedPlayerId shouldBe empty
    val nextState = module.next(initialState)
    nextState.knockedPlayerId shouldBe defined
    info("player1 knocked") 
    nextState.knockedPlayerId.get should equal (players.head.id) // player1 knocked
  }

  it should "knock if it's first round and current player's score is greater than 21 and next player cannot pick up 31 from discard pile" in {
    val hands = Seq(
      Seq(Card(Three, Clubs), Card(Nine, Clubs), Card(Jack, Clubs)),  // current player 23, greater than 21
      Seq(Card(Ten, Hearts), Card(Ace, Hearts), Card(Seven, Spades)), 
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades)))
    val discardPile = Seq(Card(Eight, Clubs)) // next player would not get 31 if picking this up
    val players = Seq(ThirtyOnePlayerState("player1", 3, hands(0)), ThirtyOnePlayerState("player2", 3, hands(1)), ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = Seq(Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), Action("player3", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    initialState.knockedPlayerId should not be defined
    val nextState = module.next(initialState)
    nextState.knockedPlayerId shouldBe defined
    info("player1 knocked") 
    nextState.knockedPlayerId.get should equal (players.head.id)
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
    val history = Seq(Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), Action("player3", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(2), discardPile = discardPile, history = history)
    initialState.knockedPlayerId should not be defined
    initialState.nextPlayerIndex() should equal (0) // next player is player1
    val nextState = module.next(initialState)
    nextState.knockedPlayerId should not be defined
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
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    initialState.knockedPlayerId should not be defined
    initialState.nextPlayerIndex() should equal (1) // next player is player 2
    val nextState = module.next(initialState)
    nextState.knockedPlayerId should not be defined // player hadn't knocked in last round
    nextState.history.length shouldBe >= (2)
    nextState.history.reverse.head should equal (Action("player1", Discard, Seq(Card(Jack, Clubs)))) // shows after current player had drawn from discard, she'd discarded the clubs card
    nextState.history.reverse.tail.head should equal (Action("player1", DrawFromDiscard, Seq(Card(Jack, Hearts)))) // shows current player had drawn from the discard pile
  }

  it should """draw from discard pile when it would increase current player's score by 7 or more, 
and next player's suspected cards would not lead to 31 from drawing discarded cards""" in {
    val hands = Seq(
      Seq(Card(Ten, Hearts), Card(Five, Hearts), Card(Ten, Clubs)), // current player, if discarding club card (ten) it would not lead to next suspected getting 31
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // next player
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades))) 
    val discardPile = Seq(Card(Seven, Hearts)) // drawing this card would make current player 7 higher than current score
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1), suspectedCards = Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades))),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = 
      Seq(
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(initialState.nextPlayerIndex() == 1) // next player is player 2
    val nextState = module.next(initialState)
    assert(!nextState.knockedPlayerId.isDefined) // player hadn't knocked in last round
    assert(nextState.history.length >= 2)
    assert(nextState.history.reverse.head == Action("player1", Discard, Seq(Card(Ten, Clubs)))) // shows after current player had drawn from discard, she'd discarded the clubs card
    assert(nextState.history.reverse.tail.head == Action("player1", DrawFromDiscard, Seq(Card(Seven, Hearts)))) // shows current player had drawn from the discard pile
  }
  
  it should """not draw from discard pile when it would increase current player's score by 6 or less""" in { 
    val hands = Seq(
      Seq(Card(Ten, Hearts), Card(Five, Hearts), Card(Ten, Clubs)), // current player, if discarding club card (ten) it would not lead to next suspected getting 31
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // next player
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades))) 
    val discardPile = Seq(Card(Six, Hearts)) // drawing this card would make current player only 6 higher than current score
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1), suspectedCards = Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades))),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = 
      Seq(
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(initialState.nextPlayerIndex() == 1) // next player is player 2
    val nextState = module.next(initialState)
    assert(!nextState.knockedPlayerId.isDefined) // player hadn't knocked in last round
    assert(nextState.history.length >= 2)
    assert(nextState.history.reverse.tail.head != Action("player1", DrawFromDiscard, Seq(Card(Seven, Hearts)))) // shows current player didn't draw from the discard pile
  }

  it should """not draw from discard pile when it would increase current player's score by 7 or more, 
but next player's suspected cards would lead to 31 from drawing discarded cards""" in {
    val hands = Seq(
      Seq(Card(Ten, Hearts), Card(Five, Hearts), Card(Ace, Clubs)), // current player, if discarding club card (ace) it would lead to next suspected getting 31
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // next player
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades))) 
    val discardPile = Seq(Card(Seven, Hearts)) // drawing this card would make current player 7 higher than current score
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1), suspectedCards = Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades))),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = 
      Seq(
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(initialState.nextPlayerIndex() == 1) // next player is player 2
    val nextState = module.next(initialState)
    assert(!nextState.knockedPlayerId.isDefined) // player hadn't knocked in last round
    assert(nextState.history.length >= 2)
    assert(nextState.history.reverse.tail.head != Action("player1", DrawFromDiscard, Seq(Card(Seven, Hearts)))) // shows current player didn't draw from the discard pile
  }

  it should """knock if current score is greater than or equal to 30 and next player wouldn't get 31 from drawing the current player discarded card, 
and next's potential score is less than current player's score""" in {
    val hands = Seq(
      Seq(Card(Ten, Hearts), Card(Jack, Hearts), Card(Queen, Hearts)), // current player has 30
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // next player
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades))) 
    val discardPile = Seq(Card(Eight, Clubs)) // next player wouldn't get 31 drawing this card, and it wouldn't be >= than current's score of 30
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1), suspectedCards = Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades))),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = 
      Seq(
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(initialState.nextPlayerIndex() == 1) // next player is player 2
    assert(!initialState.knockedPlayerId.isDefined) // no one's knocked yet
    val nextState = module.next(initialState)
    assert(nextState.knockedPlayerId.isDefined) // player knocked
    assert(nextState.knockedPlayerId.get == "player1") // player hadn't knocked in last round
    assert(nextState.history.length >= 2)
    assert(nextState.history.reverse.head == Action("player1", Knock)) // shows current player knocked and it was recorded to history
  }

  it should """not knock if current score is greater than or equal to 30 and next player wouldn't get 31 from drawing the current player discarded card, 
but next's potential score is equal to current player's score""" in {
    val hands = Seq(
      Seq(Card(Ten, Hearts), Card(Jack, Hearts), Card(Queen, Hearts)), // current player has 30
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // next player
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Ten, Spades))) 
    val discardPile = Seq(Card(Ten, Clubs)) // next player wouldn't get 31 drawing this card, but it would equal the current's score of 30
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1), suspectedCards = Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades))),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = 
      Seq(
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(initialState.nextPlayerIndex() == 1) // next player is player 2
    assert(!initialState.knockedPlayerId.isDefined) // no one's knocked yet
    val nextState = module.next(initialState)
    assert(!nextState.knockedPlayerId.isDefined) // player didn't knock 
    assert(nextState.history.length >= 2)
    assert(nextState.history.reverse.head != Action("player1", Knock)) // shows current player didn't knock in the last turn
  }

  it should """(3-of-a-kind version, giving score of 30.5) knock if current score is greater than or equal to 30 and next player wouldn't get 31 from drawing the current player discarded card, 
and next's potential score is less than current player's score""" in {
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // current player has 30.5 (3-of-a-kind)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // next player
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Eight, Spades))) 
    val discardPile = Seq(Card(Ten, Clubs)) // next player wouldn't get 31 drawing this card, and the score of 30 wouldn't be >= than current's score of 30.5
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1), suspectedCards = Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades))),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val history = 
      Seq(
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))),
        Action("player2", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player2", Discard, Seq(Card(Four, Diamonds))),
        Action("player3", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player3", Discard, Seq(Card(Four, Diamonds))),
        Action("player1", DrawFromStock, Seq(Card(Four, Diamonds))), 
        Action("player1", Discard, Seq(Card(Four, Diamonds))))
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, history = history)
    assert(initialState.nextPlayerIndex() == 1) // next player is player 2
    assert(!initialState.knockedPlayerId.isDefined) // no one's knocked yet
    val nextState = module.next(initialState)
    assert(nextState.knockedPlayerId.isDefined) // player knocked
    assert(nextState.knockedPlayerId.get == "player1") // player hadn't knocked in last round
    assert(nextState.history.length >= 2)
    assert(nextState.history.reverse.head == Action("player1", Knock)) // shows current player knocked and it was recorded to history
  }




}
