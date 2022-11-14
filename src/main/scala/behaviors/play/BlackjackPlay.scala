package cards.behaviors.play

import cards.behaviors.Commons
import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.classes.{ Card, Rank, Suit, Deck }
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.hand.Hand
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._
import cards.classes.options.BlackjackOptions
import cards.classes.options.DealerHitLimit._

trait BlackjackPlay {
  type EVAL <: BlackjackHandEvaluation 
  type COMMONS <: Commons
  val commons: COMMONS 
  val evaluation: EVAL
  import commons._
  import evaluation._

  private def pairMatchingRank(cards: Seq[Card]): Boolean = cards.length == 2 && countRank(cards).values.toSeq.contains(2)
  private def pairOfAces(cards: Seq[Card]): Boolean = pairMatchingRank(cards) && cards.head.rank == Ace

  // Players can only split on first play (only 2 cards) when both cards have same value
  // Tens, Jacks, Queens, and Kings are all considered the same value, meaning that player can split on Ten and Queen, etc... 
  // Dealers cannot split but this function doesn't check for this
  // o - game options, specifically resplitLimit and resplitOnSplitAces
  // splitCount - number of times player has split in this turn, applicable when options.resplitLimit is specified as non-Unlimitted
  // splitAcesCount - number of times player has split aces in this turn, applicable when options.resplitOnSplitAces is false
  def canSplit(cards: Seq[Card], o: BlackjackOptions = BlackjackOptions(), splitCount: Int = 0, splitAcesCount: Int = 0): Boolean = {
    
    (splitCount, o.splitLimit, o.resplitOnSplitAces, pairOfAces(cards), splitAcesCount) match {
      case (count, Some(limit), _, _, _) if (count >= limit) => false
      case (_, _, false, true, n) if (n > 0) => false // cannot split aces more than once per turn, unless variation on options allows it
      case (_, _, _, _, _) => {
        val tens: Seq[Rank] = Seq(Ten, Jack, Queen, King) // split is by value and not by rank: Ten and Jack can split even though they differ in rank
        cards.length == 2 && 
          (tens.contains(cards.head.rank) && tens.contains(cards.tail.head.rank) || pairMatchingRank(cards))
      }
    }
  }

  def getPlayerBet(playerState: BlackjackPlayerState, playerId: String): Option[(Seq[Card], Int)] = { 
    (for {
      x <- playerState.handsAndBets
      if (x.bets.keys.toSeq.contains(playerId))
    } yield (x.hand, x.bets.filter(_._1 == playerId).values.head)
    ).headOption
  }

  def getPlayerBets(game: BlackjackGameState, playerId: String): Seq[(Seq[Card], Int)] = {
    for {
      player <- game.players
      bet <- getPlayerBet(player, playerId)
    } yield bet
  }

  def hasPlayerBet(game: BlackjackGameState, playerId: String): Boolean = getPlayerBets(game, playerId) != Nil

  def handCompleted(hand: Hand): Boolean = hand.outcome.isDefined
  def handsCompleted(player: BlackjackPlayerState) = player.handsAndBets.count(h => handCompleted(h)) == player.hands.length
  def isFirstRound(hand: Hand): Boolean = hand.outcome.isEmpty && hand.hand.length == 2

  def betsHaveBeenPlaced(game: BlackjackGameState): Boolean = {
    (for {
      player <- game.players
    } yield hasPlayerBet(game, player.id)).count(p => p) == game.players.length
  }

  def isTimeToPlay(game: BlackjackGameState): Boolean = {
    if (game.players == Nil)
      throw new IllegalArgumentException("cannot determine whether isTimeToPlay when there are no players")

    val hands: Seq[Seq[Card]] = game.players.flatMap(p => p.hands)
    hands.count(cs => cs.length >= 2) == hands.length &&
    betsHaveBeenPlaced(game) && 
      (game.players.flatMap(_.handsAndBets.map(h => h.outcome)).count(w => w.isDefined) != game.players.length)
  }

  def isTimeToDeal(game: BlackjackGameState): Boolean = {
    // time to deal if either dealer doesn't have all cards or 1 or more players doesn't have all cards
    game.players.length > 0 && 
      (game.dealerHand.hand.length < 2 ||
        game.players.count(p => p.hands == Nil || p.hands.count(_.length < 2) > 0) > 0  ) // then it's time to deal
  }

  def deal(game: BlackjackGameState): BlackjackGameState = {
    if (!isTimeToDeal(game)) {
      throw new IllegalArgumentException("cannot deal cards because it's not currently time to deal")
    }
    var deck = game.deck
    val (updatedPlayersAndHistories): Seq[(BlackjackPlayerState, Option[Action[BlackjackAction]])] = (for {
      player <- game.players
      hand <- player.handsAndBets
    } yield {
      val (updatedCards, updatedDeck): (Seq[Card], Deck) = deck.deal(2 - hand.hand.length)
      deck = updatedDeck
      val history = if (hand.hand.length < 2) 
        // Some(Action(player.id, IsDealt, updatedCards, 0, hand.hand, Seq(hand.hand ++ updatedCards) ))
        Some(Action(player.id, IsDealt, updatedCards, None ))
      else
        None
      (player.updateHand(hand.hand, updatedCards), history) // only updates hand when updatedCards isn't empty
    })
    val (newlyDealtDealerCards, updatedDeck): (Seq[Card], Deck) = game.dealerHand.hand.length match {
      case n if (n < 2) => deck.deal(2 - n)
      case _ => (Nil, deck)
    } 
    val updatedDealerHand: Seq[Card] = game.dealerHand.hand ++ newlyDealtDealerCards
    val originalDealerWithFaceDown: Seq[Card] = game.dealerHand.hand.length match {
      case 0 => Nil
      case 1 => Seq(Card.FaceDownCard)
      case _ => Seq(Card.FaceDownCard) ++ game.dealerHand.hand.tail
    }
    val newDealerWithFaceDown: Seq[Card] = Seq(Card.FaceDownCard) ++ updatedDealerHand.tail
    val dealerHistory: Seq[Action[BlackjackAction]] = newlyDealtDealerCards.length match {
      case 0 => Nil
      case _ => {
        Seq(Action("Dealer", IsDealt, newDealerWithFaceDown, None))
      }
    }
    val history: Seq[Action[BlackjackAction]] = updatedPlayersAndHistories.filter(_._2.isDefined).map(_._2.get) ++ dealerHistory
    val updatedPlayers: Seq[BlackjackPlayerState] = updatedPlayersAndHistories.map(_._1) 
    assert(updatedDeck != game.deck) 
    game.copy(players = updatedPlayers, deck = updatedDeck, dealerHand = game.dealerHand.copy(hand = updatedDealerHand), history = game.history ++ history) 
  }

  def isTimeForDealerToPlay(game: BlackjackGameState): Boolean = {
    if (game.players == Nil)
      throw new IllegalArgumentException("cannot determine whether isTimeForDealerToPlay when there are no players")

    game.currentPlayerIndex.isEmpty && // only time for dealer to play when no current player is still playing his or her hand
      isTimeToPlay(game) && // play time
      !isTimeToDeal(game) // && // not time to deal
      // playerHandCounts == playerCompletedHandCounts // all hands have either busted or are standing or surrendering
  }

  def dealerPlay(game: BlackjackGameState): BlackjackGameState = {
    if (!isTimeForDealerToPlay(game)) {
      throw new IllegalArgumentException("dealer cannot play hand because it's not currently time for dealer to play")
    }
    val action: BlackjackAction = 
      (evaluation.eval(game.dealerHand.hand), game.options.dealerHitLimit, game.dealerHand.hand.map(_.rank).contains(Ace)) match {
        case (17, H17, true) => Hit // dealer hits on soft 17
        case (17, S17, true) => Stand // dealer stands on soft 17
        case (n, _, _) if (n > 17) => Stand
        case (_, _, _) => Hit
      }
    val (newDealerCards, newHistory, newDeck): (Seq[Card], Seq[Action[BlackjackAction]], Deck) = action match {
      case Stand => (game.dealerHand.hand, Seq(Action("Dealer", Stand, Nil, None, Nil, Seq(Seq(Card.FaceDownCard) ++ game.dealerHand.hand.tail))), game.deck)
      case Hit => {
        // Hit deals 1 card 
        val (dealt, nextDeck): (Seq[Card], Deck) = game.deck.deal()
        (game.dealerHand.hand ++ dealt, Seq(Action("Dealer", Hit, dealt, None, Nil, Seq(Seq(Card.FaceDownCard) ++ (game.dealerHand.hand ++ dealt).tail))), nextDeck)
      }
      case a => throw new IllegalArgumentException(s"Unexpected dealer action [$a]; dealer can only ever Hit or Stand")
    }
    val nextState: BlackjackGameState = game.copy(
      deck = newDeck, 
      history = game.history ++ newHistory, 
      dealerHand = game.dealerHand.copy(hand = newDealerCards))

    // TODO: test
    // if dealer's 21 or busted or is Standing, then game is over and bets should be settled
    val gameOver: Boolean = eval(newDealerCards) >= 21 || action == Stand || nextState.history.reverse.head.action == ShowCards
    gameOver match {
      case false => nextState
      case true => { 
        val dealerShowCards = Action("Dealer", ShowCards, newDealerCards) 
        evaluation.outcomes(nextState.copy(history = nextState.history ++ Seq(dealerShowCards))) // game over: evaluate each hand against dealer's to prepare to settleBets
      }
    }
  }

  // Basic Strategy in Blackjack 
  def nextAction(game: BlackjackGameState): BlackjackAction = {
    if (game.players == Nil) {
      throw new IllegalArgumentException("Cannot play current hand because there are no players") 
    }
    if (game.currentPlayerIndex.isEmpty) {
      throw new IllegalArgumentException("Cannot play current hand because current player is not specified")
    }
    if (game.currentHandIndex.isEmpty) {
      throw new IllegalArgumentException("Cannot play current hand because current hand is not specified")
    }
    if (game.currentCards().length < 2) {
      throw new IllegalArgumentException(s"Cannot play current hand because current hand length [${game.currentCards().length}] is less than length 2")
    }
    if (game.dealerHand.hand.length != 2) {
      throw new IllegalArgumentException(s"Cannot play current hand because dealer's hand length [${game.dealerHand.hand.length}] is not length 2")
    }
    if (!isTimeToPlay(game)) {
      throw new IllegalArgumentException(s"Cannot play current hand because it's not currently time to play hand")
    }
    val canDoubleDown: Boolean = game.currentCards().length == 2
    // we only care about this current player's actions
    val previousActions: Seq[Action[BlackjackAction]] = game.history.filter(_.playerId == game.currentPlayer().id)
    // current player's split count from history
    val splitCount: Int = previousActions.count(a => a.action == Split)
    // current player's aces split count from history
    val acesSplitCount: Int = previousActions.count(a => pairOfAces(a.beforeCards) && a.action == Split) 
    val eligibleToSplit: Boolean = canSplit(game.currentCards(), game.options, splitCount, acesSplitCount)
    
    // player's turn: based on player's cards and dealers face up card, decide which action to take
    // looking at cards in reverse order so aces are at head, and changed to list in order to pattern match on head :: tail
    val highestRank: Rank = game.currentCards().sorted.reverse.head.rank
    val totalScore: Int = eval(game.currentCards())
    val tailScore: Int = eval(game.currentCards().tail)
    // val dealerFaceUpRank: Rank = game.dealerHand.hand.head.rank
    val dealerFaceUpRank: Rank = game.dealerHand.hand.tail.head.rank
    val surrenderOffered: Boolean = game.options.allowSurrender 
    val tens: Seq[Rank] = Seq(Ten, Jack, Queen, King) // ranks which have value of 10

    // this logic is Basic Strategy in Blackjack 
    (highestRank, tailScore, totalScore, dealerFaceUpRank, canDoubleDown, eligibleToSplit, surrenderOffered) match {
      // **** case 1: pairs
      // eligible to split on aces, do it regardless of dealer's showing card
      case (Ace, _, _, _, _, true, _) => Split
      // always Stand on 20, regardless whether Split is available
      case (_, 20, _, _, _, _, _) => Stand
      // case (rank, _, _, _, _, true, _) if (tens.contains(rank)) => Stand
      // stand on 18 when dealer shows 7, 10, or Ace
      case (Nine, _, _, dealer, _, true, _) if (Seq(Seven, Ten, Ace).contains(dealer)) => Stand
      // else split on 18 when dealer shows anything else
      case (Nine, _, _, _, _, true, _) => Split
      // surrender on 16 when it's an available option and dealer shows Ace
      case (Eight, _, _, Ace, _, true, true) => Surrender
      // dealer shows Ace but surrender is not allowed, so Split eights
      case (Eight, _, _, Ace, _, true, false) => Split
      // if dealer shows 8 or higher then Hit on 14
      case (Seven, _, _, dealer, _, true, _) if ((Seq(Eight, Nine, Ace) ++ tens).contains(dealer)) => Hit
      // else dealer shows 7 or lower, so Split on 2 Sevens
      case (Seven, _, _, _, _, true, _) => Split
      // if dealer shows 7 or higher then Hit on 12
      case (Six, _, _, dealer, _, true, _) if ((Seq(Seven, Eight, Nine, Ace) ++ tens).contains(dealer)) => Hit
      // else dealer shows 6 or lower, so Split on 2 Sixes
      case (Six, _, _, _, _, true, _) => Split
      // if dealer shows 10 or higher then Hit on 10
      case (Five, _, _, dealer, _, true, _) if ((Seq(Ace) ++ tens).contains(dealer)) => Hit
      // dealer shows 9 or lower and double down is offered so double down on 10
      case (Five, _, _, _, true, true, _) => DoubleDown
      // dealer shows 9 or lower but doubling down isn't available, so Hit on 10
      case (Five, _, _, _, true, false, _) => Hit
      // dealer shows 5 or 6 so Split on 2 Fours
      case (Four, _, _, dealer, _, true, _) if (Seq(Five, Six).contains(dealer)) => Split
      // else dealer does not show 5 or 6 so Hit on 8
      case (Four, _, _, _, _, true, _) => Hit
      // dealer shows 8 or higher so hit on 6
      case (Three, _, _, dealer, _, true, _) if ((Seq(Eight, Nine, Ace) ++ tens).contains(dealer)) => Hit
      // else dealer shows 7 or lower so Split 2 Threes
      case (Three, _, _, _, _, true, _) => Split
      // dealer shows 8 or higher so hit on 4 
      case (Two, _, _, dealer, _, true, _) if ((Seq(Eight, Nine, Ace) ++ tens).contains(dealer)) => Hit
      // else dealer shows 7 or lower so Split 2 Twos
      case (Two, _, _, _, _, true, _) => Split
      // **** case 2: player has a soft total
      // soft total of 20 should Stand
      case (Ace, 9, _, _, _, _, _) => Stand
      // if hand is 19 and dealer shows 6, double down (if it's an available option) 
      case (Ace, 8, _, Six, true, _, _) => DoubleDown
      // else Stand on 19 when double down is unavailable, regardless what dealer shows
      case (Ace, 8, _, _, _, _, _) => Stand
      // Hit on 18 when dealer shows Nine or higher 
      case (Ace, 7, _, dealer, _, _, _) if ((Seq(Nine, Ace) ++ tens).contains(dealer)) => Hit
      // Stand on 18 when dealer shows Seven or Eight
      case (Ace, 7, _, dealer, _, _, _) if (Seq(Seven, Eight).contains(dealer)) => Stand
      // Double-down if it's available on 18 when dealer shows Six or lower 
      case (Ace, 7, _, _, true, _, _) => DoubleDown
      // else Double-down is not an option, so Stand on 18 when dealer shows Six or lower
      case (Ace, 7, _, _, false, _, _) => Stand
      // Double-down if it's available on a 17 when dealer shows 3, 4, 5, or 6
      case (Ace, 6, _, dealer, true, _, _) if (Seq(Three, Four, Five, Six).contains(dealer)) => DoubleDown
      // else Double-down is unavailable, so Hit on 17
      case (Ace, 6, _, _, false, _, _) => Hit
      // Double-down if it's available on a 15 or 16 when dealer shows 4, 5, or 6
      case (Ace, tail, _, dealer, true, _, _) if (Seq(Four, Five, Six).contains(dealer) && (tail == 5 || tail == 4)) => DoubleDown
      // else Double-down is unavailable, so Hit on 16
      case (Ace, tail, _, _, false, _, _) if (tail == 5 || tail == 4) => Hit
      // Double-down if it's available on either 13 or 14 when dealer shows 5 or 6
      case (Ace, tail, _, dealer, true, _, _) if (Seq(Five, Six).contains(dealer) && (tail == 3 || tail == 2)) => DoubleDown
      // else Double-down is unavailable, so Hit on either 13 or 14
      case (Ace, tail, _, _, false, _, _) if (tail == 3 || tail == 2) => Hit
      // **** case 3: hard-totals (excluding pairs)
      // Stand on 18-21
      case (_, _, hand, _, _, _, _) if (Seq(18, 19, 20, 21).contains(hand)) => Stand
      // if dealer shows Ace then Surrender if available on 17 
      case (_, _, 17, Ace, _, _, true) => Surrender
      // else Surrender is not offered, so Stand on 17 when dealer shows Ace
      case (_, _, 17, Ace, _, _, false) => Stand
      // dealer doesn't show Ace, so Stand on 17 
      case (_, _, 17, _, _, _, _) => Stand
      // if dealer shows 9 or higher, Surrender on 16 if option is available
      case (_, _, 16, dealer, _, _, true) if ((Seq(Nine, Ace) ++ tens).contains(dealer)) => Surrender
      // else Surrender is not offered when dealer shows 9 or higher, so Hit on 16
      case (_, _, 16, dealer, _, _, false) if ((Seq(Nine, Ace) ++ tens).contains(dealer)) => Hit
      // if dealer shows 7 or 8 then Hit on 16
      case (_, _, 16, dealer, _, _, _) if (Seq(Seven, Eight).contains(dealer)) => Hit
      // else dealer shows 6 or lower so Stand 16
      case (_, _, 16, _, _, _, _) => Stand
      // if dealer shows 10 or higher, Surrender on 15 if option is available
      case (_, _, 15, dealer, _, _, true) if ((Seq(Ace) ++ tens).contains(dealer)) => Surrender
      // else Surrender is not offered when dealer shows 10 or higher, so Hit on 15
      case (_, _, 15, dealer, _, _, false) if ((Seq(Ace) ++ tens).contains(dealer)) => Hit
      // if dealer shows 7, 8, or 9 then Hit on 15
      case (_, _, 15, dealer, _, _, _) if (Seq(Seven, Eight, Nine).contains(dealer)) => Hit
      // else dealer shows 6 or lower so Stand 15
      case (_, _, 15, _, _, _, _) => Stand
      // if dealer shows 7 or higher and hand is either 13 or 14 then Hit
      case (_, _, hand, dealer, _, _, _) if (Seq(13, 14).contains(hand) && ((Seq(Seven, Eight, Nine, Ace) ++ tens).contains(dealer))) => Hit
      // else dealer shows 6 or less so Stand on either 13 or 14
      case (_, _, hand, _, _, _, _) if (Seq(14, 15).contains(hand)) => Stand
      // if dealer shows 4, 5, or 6 then Stand on 12 
      case (_, _, 12, dealer, _, _, _) if (Seq(Four, Five, Six).contains(dealer)) => Stand
      // else dealer Hit on 12 when dealer shows anything other than 4, 5, or 6
      case (_, _, 12, _, _, _, _) => Hit
      // Double-down if available on 11
      case (_, _, 11, _, true, _, _) => DoubleDown
      // Hit on 11 if Double-down is not an option
      case (_, _, 11, _, false, _, _) => Hit
      // Hit on 10 if dealer shows 10 or 11 
      case (_, _, 10, dealer, _, _, _) if ((Seq(Ace) ++ tens).contains(dealer)) => Hit
      // else dealer shows 9 or less, if the option is available then player should Double-down on 10 
      case (_, _, 10, _, true, _, _) => DoubleDown
      // the Double-down option is unavailable so Hit on 10 when dealer is 9 or less
      case (_, _, 10, _, false, _, _) => Hit
      // if Double-down option is available and dealer has 3, 4, 5 or 6 then Double-down on 9
      case (_, _, 9, dealer, true, _, _) if (Seq(Three, Four, Five, Six).contains(dealer)) => DoubleDown
      // else if Double-down option is unavailable then hit on 9 
      case (_, _, 9, _, _, _, _) => Hit
      // else hit on 8 or lower
      case (_, _, _, _, _, _, _) => Hit
    }
  }

  // Current player plays current hand
  // Note that this does not increment game's player index nor the current player's hand index (not this function's responsibility) 
  def playHand(game: BlackjackGameState): BlackjackGameState = {
    if (game.players == Nil) {
      throw new IllegalArgumentException("Cannot play current hand because there are no players") 
    }
    if (game.currentPlayerIndex.isEmpty) {
      throw new IllegalArgumentException("Cannot play current hand because current player is not specified")
    }
    if (game.currentHandIndex.isEmpty) {
      throw new IllegalArgumentException("Cannot play current hand because current hand is not specified")
    }
    if (game.currentCards().length < 2) {
      throw new IllegalArgumentException(s"Cannot play current hand because current hand length [${game.currentCards().length}] is less than length 2")
    }
    if (game.dealerHand.hand.length != 2) {
      throw new IllegalArgumentException(s"Cannot play current hand because dealer's hand length [${game.dealerHand.hand.length}] is not length 2")
    }
    if (!isTimeToPlay(game)) {
      throw new IllegalArgumentException(s"Cannot play current hand because it's not currently time to play hand")
    }
    // next action based on Basic Strategy in Blackjack 
    val action: BlackjackAction = nextAction(game)
    // perform the action
    val (outcomeHands, updatedDeck, newHistory): (Seq[Hand], Deck, Seq[Action[BlackjackAction]]) = 
      performPlayAction(game.currentPlayer().id, action, game.currentHand(), game.deck)
    
    // check for an additional hand (from splitting)  
    val (updatedCurrentHand, splitHand): (Hand, Option[Hand]) = outcomeHands.length match {
      case 1 => (outcomeHands.head, None)
      case _ => (outcomeHands.head, Some(outcomeHands.tail.head))
    }
    // current player's updated hands (hand added via split will be added momentarly, hence the mutable)
    var updatedHands: Seq[Hand] = 
      game.currentPlayer().handsAndBets.map { hand => hand.hand match {
        case cs if (cs == game.currentCards()) => updatedCurrentHand
        case _ => hand
      }
    }
    // check whether an additional hand needs to be added (due to split)
    updatedHands = splitHand match {
      case Some(h) => updatedHands ++ Seq(h) // split occurred, add new hand
      case None => updatedHands 
    }

    // yield updated game state (but player's current hand or current player will not increment; not this function's responsibility) 
    game.copy(deck = updatedDeck, history = game.history ++ newHistory, players = for (p <- game.players) yield {
      if (p == game.currentPlayer())
        p.copy(handsAndBets = updatedHands)
      else p 
    }).toNextHand(action, evaluation.eval(outcomeHands.head.hand) > 21)
    // if player Stands or Surrenders, or if current hand busted, then iterate to next hand, or if it doesn't exist then to the next player
  }

  // returns updated cards (seq of hands to account for Splits), updated deck, and new history
  def performPlayAction(
    playerId: String, 
    action: BlackjackAction, 
    hand: Hand, 
    deck: Deck): (Seq[Hand], Deck, Seq[Action[BlackjackAction]]) = action match {
      // case Stand => (Seq(hand), deck, Seq(Action(playerId, Stand, Nil, 0, hand.hand, Seq(hand.hand))))
      case Stand => (Seq(hand), deck, Seq(Action(playerId, Stand, Nil, None, Nil, Seq(hand.hand))))
      case a if (a == Hit || a == DoubleDown) => {
        // Hit and DoubleDown both deal 1 card 
        val (dealt, nextDeck): (Seq[Card], Deck) = deck.deal()
        // bets is potentially modified (if player doubled-down) 
        val (additionalBet, nextBets): (Int, Map[String, Int]) = a match {
          case DoubleDown => {
            ( // additional bet is same that player already had bet (doubling it)
              hand.bets(playerId),
              // bets adjusted to double player's bet 
              (for ((player, bet) <- hand.bets) yield {
                if (player == playerId)
                  player -> bet * 2
                else 
                  player -> bet
              }).toMap)
          }
          case _ => (0, hand.bets)
        }
        val updatedHand: Hand = hand.copy(hand = hand.hand ++ dealt, bets = nextBets)
        // (Seq(updatedHand), nextDeck, Seq(Action(playerId, a, dealt, additionalBet, hand.hand, Seq(updatedHand.hand))))
        (Seq(updatedHand), nextDeck, Seq(Action(playerId, a, dealt, Some(additionalBet), Nil, Seq(updatedHand.hand))))
      }
      case Split => {
        // deal 2 cards, 1 for each split hand 
        val (dealt, nextDeck): (Seq[Card], Deck) = deck.deal(2)
        val hand1: Hand = Hand(Seq(hand.hand.head, dealt.head), hand.bets)
        val hand2: Hand = Hand(Seq(hand.hand.tail.head, dealt.tail.head), hand.bets)
        // original bet amount is added as as new bet to the new hand 
        val additionalBet: Int = hand.bets(playerId)
        // (Seq(hand1, hand2), nextDeck, Seq(Action(playerId, Split, dealt, additionalBet, hand.hand, Seq(hand1, hand2).map(_.hand)))) 
        (Seq(hand1, hand2), nextDeck, Seq(Action(playerId, Split, dealt, Some(additionalBet), Nil, Seq(hand1, hand2).map(_.hand)))) 
      }
      case Surrender => {
        val (surrenderAmount, nextBets): (Int, Map[String, Int]) = {
          (
            hand.bets(playerId) / 2, // surrender bet is half original bet
            // bets adjusted to halve player's bet 
            (for ((player, bet) <- hand.bets) yield {
              if (player == playerId)
                player -> bet / 2 // halved 
              else 
                player -> bet
            }).toMap
          )
        }
        // afterwards player does not have any cards in the hand since player has surrendered; bet is adjusted so player only pays half
        (Seq(hand.copy(hand = Nil, bets = nextBets)), deck, Seq(Action(playerId, Surrender, Nil, Some(surrenderAmount), hand.hand, Nil)))
      }
      case a => throw new IllegalArgumentException(s"Unexpected BlackjackAction [$a], at this phase the only expected actions are [Hit, Stand, DoubleDown, Split, Surrender]")
    }


}