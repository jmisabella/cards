package cards.behaviors.controller

import cards.behaviors.Commons
import cards.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.behaviors.controller.ThirtyOneController
import cards.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.classes.DeckType._
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.state.{ ThirtyOneGameState, ThirtyOnePlayerState }
import cards.classes.actions.{ Action, ThirtyOneAction }
import cards.classes.actions.ThirtyOneAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class ThirtyOneControllerSpec extends AnyFlatSpec with GivenWhenThen {
  private [controller] case object _commons extends Commons
  private [controller] case object _evaluation extends ThirtyOneHandEvaluation {
    override type C = Commons
    override val commons = _commons
  }
  case object module extends ThirtyOneController {
    override type EVAL = ThirtyOneHandEvaluation
    override val evaluation = _evaluation
  }

  "ThirtyOneController" should "throw IllegalStateException when calling next() from game state which does not yet have any player states" in {
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
    initialState.nextPlayerIndex() should equal (1) // next player is player 2
    val nextState = module.next(initialState)
    nextState.knockedPlayerId should not be defined // player hadn't knocked in last round
    nextState.history.length should be >= (2)
    nextState.history.reverse.head should equal (Action("player1", Discard, Seq(Card(Ten, Clubs)))) // shows after current player had drawn from discard, she'd discarded the clubs card
    nextState.history.reverse.tail.head should equal (Action("player1", DrawFromDiscard, Seq(Card(Seven, Hearts)))) // shows current player had drawn from the discard pile
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
    initialState.nextPlayerIndex() should equal (1) // next player is player 2
    val nextState = module.next(initialState)
    nextState.knockedPlayerId should not be defined // player hadn't knocked in last round
    nextState.history.length should be >= (2)
    nextState.history.reverse.tail.head should not equal (Action("player1", DrawFromDiscard, Seq(Card(Seven, Hearts)))) // shows current player didn't draw from the discard pile
    nextState.history.reverse.tail.head.action should equal (DrawFromStock)
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
    initialState.nextPlayerIndex() should equal (1) // next player is player 2
    val nextState = module.next(initialState)
    nextState.knockedPlayerId should not be defined // player hadn't knocked in last round
    nextState.history.length should be >= (2)
    nextState.history.reverse.tail.head should not equal (Action("player1", DrawFromDiscard, Seq(Card(Seven, Hearts)))) // shows current player didn't draw from the discard pile
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
    initialState.nextPlayerIndex() should equal (1) // next player is player 2
    initialState.knockedPlayerId should not be defined // no one's knocked yet
    val nextState = module.next(initialState)
    nextState.knockedPlayerId shouldBe defined // player knocked
    nextState.knockedPlayerId should contain ("player1") // player hadn't knocked in last round
    nextState.history.length shouldBe >= (2)
    nextState.history.reverse.head should equal (Action("player1", Knock)) // shows current player knocked and it was recorded to history
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
    initialState.nextPlayerIndex() should equal (1) // next player is player 2
    initialState.knockedPlayerId should not be defined // no one's knocked yet
    val nextState = module.next(initialState)
    nextState.knockedPlayerId should not be defined // player didn't knock 
    nextState.history.length should be >= (2)
    nextState.history.reverse.head should not equal (Action("player1", Knock)) // shows current player didn't knock in the last turn
  }

  it should """(blitz) make player who got a 31 before anyone's knocked the winner, with all other players paying 1 token""" in {
    Given("a game state where a winner's been declared and a player has 31, but no player has knocked") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // 3-of-a-kind is 30.5 score but is not 31, so would have to pay since no knocker
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Ace, Clubs)), // score of 31, the winner
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Eight, Spades)))  // current player: another non-31 hand must also pay 1 token 
    val discardPile = Seq(Card(Ten, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1)),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(2), discardPile = discardPile, winningPlayerId = Some("player2"))
    initialState.knockedPlayerId should not be defined // nobody's knocked in this scenario
    
    When("calling next on the state")
    val paymentSettled = module.next(initialState)
    
    Then("the winner flag would be disabled and all players other than the winner (who blitzed with 31) pay 1") 
    paymentSettled.history.reverse.head should (equal (Action("player1", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(1))))
    paymentSettled.history.reverse.tail.head should (equal (Action("player1", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(1))))
    paymentSettled.history.count(a => a.playerId == "player2" && a.action == Pay) equals (0)
    paymentSettled.winningPlayerId shouldBe empty
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
    initialState.nextPlayerIndex() should equal (1) // next player is player 2
    initialState.knockedPlayerId should not be defined // no one's knocked yet
    val nextState = module.next(initialState)
    nextState.knockedPlayerId shouldBe defined // player knocked
    nextState.knockedPlayerId should contain ("player1") // player hadn't knocked in last round
    nextState.history.length shouldBe >= (2)
    nextState.history.reverse.head should equal (Action("player1", Knock)) // shows current player knocked and it was recorded to history
  }

  it should "make the lowest player pay 1 token when a winner's been declared (nobody's knocked)" in {
    Given("a game state where a winner's been declared and there's only 1 hand which is the lowest (nobody's knocked)") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // highest hand (3-of-a-kind is 30.5 score)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // score of 20, the lowest score
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Eight, Spades)))  // score of 25
    val discardPile = Seq(Card(Ten, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1)),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, winningPlayerId = Some("player2"))
    initialState.knockedPlayerId should not be defined // nobody's knocked in this scenario
    
    When("calling next on the state")
    val paymentSettled = module.next(initialState)
    
    Then("the winner flag would be disabled and the lowest player would have pay 1 token") 
    println("HERE: " + paymentSettled.history.reverse.head) 
    paymentSettled.history.reverse.head should equal (Action("player2", Pay, Nil, Some(1)) )
    paymentSettled.winningPlayerId shouldBe empty
  }

  it should "make the lowest player pay 1 token when a winner's been declared (a different player's knocked)" in {
    Given("a game state where a winner's been declared and there's only 1 hand which is the lowest (a different player's knocked)") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // highest hand (3-of-a-kind is 30.5 score)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // score of 20, the lowest score
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Eight, Spades)))  // score of 25
    val discardPile = Seq(Card(Ten, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1)),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    info("knocked player is not the losing player")
    val initialState = ThirtyOneGameState(
      players = players, 
      currentPlayerIndex = Some(0), 
      discardPile = discardPile, 
      knockedPlayerId = Some("player1"), 
      winningPlayerId = Some("player2"))
    initialState.knockedPlayerId shouldBe defined // somebody's knocked in this scenario
    
    When("calling next on the state")
    val paymentSettled = module.next(initialState)
    
    Then("the winner and knocked flags would both be disabled and the lowest player would have pay 1 token") 
    paymentSettled.history.reverse.head should equal (Action("player2", Pay, Nil, Some(1))) 
    paymentSettled.players.filter(_.id == "player2").head.tokens should equal (2) 
    paymentSettled.winningPlayerId shouldBe empty
  }


  it should "make the 2 players whose hands tie for lowest hands pay 1 token when a winner's been declared" in {
    Given("a game state where a winner's been declared and there's two tied hands with lowest scores (nobody's knocked)") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // highest hand (3-of-a-kind is 30.5 score)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // score of 20, tied for the lowest score
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Three, Spades)))  // score of 20, tied for the lowest score
    val discardPile = Seq(Card(Ten, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1)),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, winningPlayerId = Some("player2"))
    initialState.knockedPlayerId should not be defined // nobody's knocked in this scenario

    When("calling next on the state")
    val paymentSettled = module.next(initialState)

    Then("both players with tied lowest hands pay 1 token each")
    paymentSettled.history.length shouldBe >= (2)
    info("last 2 history actions should be player2 and player3 both paying 1 token")
    paymentSettled.history.reverse.head should (equal (Action("player2", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(1))))
    paymentSettled.history.reverse.tail.head should (equal (Action("player2", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(1))))
  }

  it should "make the knocker pay 2 tokens when their hand is the lowest and a winner's been declared" in {
    Given("a game state where a winner's been declared and the single lowest score is the knocker's, who must pay double") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // highest hand (3-of-a-kind is 30.5 score)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // score of 20
      Seq(Card(Eight, Spades), Card(Four, Spades), Card(Three, Spades)))  // score of 15, lowest score
    val discardPile = Seq(Card(Ten, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1)),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, knockedPlayerId = Some("player3"), winningPlayerId = Some("player1"))
    initialState.knockedPlayerId shouldBe defined // player3 had knocked

    When("calling next on the state")
    val paymentSettled = module.next(initialState)
    
    Then("the lone loser who is also the knocker must pay double (2) tokens")
    paymentSettled.history.reverse.head should equal (Action("player3", Pay, Nil, Some(2)) )
    paymentSettled.players.filter(_.id == "player3").head.tokens should equal (1) 
  }

  it should "make the knocker pay 2 tokens and the other player whose hand ties as lowest pay 1 token" in {
    Given("a game state where a winner's been declared and there's a tie for lowest score, with one of the losers the kocker who pay's double") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // highest hand (3-of-a-kind is 30.5 score)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // score of 20, tied at lowest score, but didn't knock so pays 1 token
      Seq(Card(Ten, Spades), Card(King, Spades), Card(Three, Hearts)))  // score of 20, tied at lowest score, but knocked so pays double
    val discardPile = Seq(Card(Four, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 3, hands(0)), 
        ThirtyOnePlayerState("player2", 3, hands(1)),
        ThirtyOnePlayerState("player3", 3, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, knockedPlayerId = Some("player3"), winningPlayerId = Some("player1"))
    initialState.knockedPlayerId shouldBe defined // player3 had knocked

    When("calling next on the state")
    val paymentSettled = module.next(initialState)
    
    Then("loser who is the knocker must pay double (2) tokens, and  the other tied loser pays 1 token")
    paymentSettled.history.reverse.head should (equal (Action("player2", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(2))) )
    paymentSettled.history.reverse.tail.head should (equal (Action("player2", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(2))) )
    info("player1 didn't pay so should have all 3 tokens")
    paymentSettled.players.filter(_.id == "player1").head.tokens should equal (3)
    info("player2 payed 1 so should have 2 tokens remaining")
    paymentSettled.players.filter(_.id == "player2").head.tokens should equal (2)
    info("player3 payed 2 so should have only 1 token remaining")
    paymentSettled.players.filter(_.id == "player3").head.tokens should equal (1)
  }

  it should "make a player who runs out of tokens leave the game" in {
    Given("a game state where a winner's been declared and there's only 1 hand which is the lowest and this player only has 1 remaining token") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // highest hand (3-of-a-kind is 30.5 score)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // score of 20, the lowest score
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Eight, Spades)))  // score of 25
    val discardPile = Seq(Card(Ten, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 1, hands(0)), 
        ThirtyOnePlayerState("player2", 1, hands(1)),
        ThirtyOnePlayerState("player3", 1, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, winningPlayerId = Some("player2"))
    initialState.knockedPlayerId should not be defined // nobody's knocked in this scenario

    When("calling next on the state")
    val paymentSettled = module.next(initialState)

    Then("both players with tied lowest hands pay 1 token each")
    paymentSettled.history.length shouldBe >= (2)
    info("last 2 history actions should be player2 paying 1 token and player2 losing out")
    
    paymentSettled.history.reverse.head should equal (Action("player2", Out, Nil))
    paymentSettled.history.reverse.tail.head should equal (Action("player2", Pay, Nil, Some(1)))
    paymentSettled.players.map(_.id) should not contain "player2" 
    paymentSettled.players.map(_.id) contains "player1" 
    paymentSettled.players.map(_.id) contains "player3" 
  }

  it should "make multiple players who run out of tokens leave the game" in {
    Given("a game state where a winner's been declared and there's 2 hands which tie for the lowest and all players only have 1 remaining token") 
    val hands = Seq(
      Seq(Card(Seven, Hearts), Card(Seven, Spades), Card(Seven, Diamonds)), // highest hand (3-of-a-kind is 30.5 score)
      Seq(Card(Queen, Clubs), Card(Jack, Clubs), Card(Seven, Spades)), // score of 20, tied for lowest score
      Seq(Card(Eight, Spades), Card(Nine, Spades), Card(Three, Spades)))  // score of 20, tied for lowest score
    val discardPile = Seq(Card(Ten, Clubs))
    val players = 
      Seq(
        ThirtyOnePlayerState("player1", 1, hands(0)), 
        ThirtyOnePlayerState("player2", 1, hands(1)),
        ThirtyOnePlayerState("player3", 1, hands(2))) 
    val initialState = ThirtyOneGameState(players = players, currentPlayerIndex = Some(0), discardPile = discardPile, winningPlayerId = Some("player2"))
    initialState.knockedPlayerId should not be defined // nobody's knocked in this scenario

    When("calling next on the state")
    val paymentSettled = module.next(initialState)

    Then("both players with tied lowest hands pay 1 token each")
    paymentSettled.history.length shouldBe >= (4)
    info("last 2 history actions should be player2 paying 1 token and player2 losing out")
    
    paymentSettled.history.reverse.head should (equal (Action("player2", Out, Nil)) or equal (Action("player3", Out, Nil)) )
    paymentSettled.history.reverse.tail.head should (equal (Action("player2", Out, Nil)) or equal (Action("player3", Out, Nil)) )
    paymentSettled.history.reverse.tail.tail.head should (equal (Action("player2", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(1))) )
    paymentSettled.history.reverse.tail.tail.tail.head should (equal (Action("player2", Pay, Nil, Some(1))) or equal (Action("player3", Pay, Nil, Some(1))) )
    paymentSettled.players.map(_.id) should not contain "player2" 
    paymentSettled.players.map(_.id) contains "player1" 
    paymentSettled.players.map(_.id) should not contain "player3" 
  }

  it should "have the discarded card always be one of the previous player's cards unless they've knocked" in {
    Given("A game state in which current player would draw from discard pile because doing so would increase hand by 7 and discarded card would not lead to next player getting a 31") 
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
    initialState.nextPlayerIndex() should equal (1) // next player is player 2

    When("calling for next state")
    val nextState = module.next(initialState)

    Then("top card of discard pile should be Ten of Clubs and the discard pile's size should remain unchanged")
    nextState.knockedPlayerId should not be defined // player hadn't knocked in last round
    nextState.history.length should be >= (2)
    nextState.history.reverse.head should equal (Action("player1", Discard, Seq(Card(Ten, Clubs)))) // shows after current player had drawn from discard, she'd discarded the clubs card
    nextState.history.reverse.tail.head should equal (Action("player1", DrawFromDiscard, Seq(Card(Seven, Hearts)))) // shows current player had drawn from the discard pile
    nextState.discardPile.length should equal (initialState.discardPile.length)
    nextState.discardPile.head should equal (Card(Ten, Clubs))
  }
}
