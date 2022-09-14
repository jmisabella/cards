package cards.models.behaviors.play

import cards.models.behaviors.Commons
import cards.models.behaviors.evaluation.BlackjackHandEvaluation
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import cards.models.classes.options.BlackjackOptions

trait BlackjackPlay {
  type EVAL <: BlackjackHandEvaluation 
  type COMMONS <: Commons
  val commons: COMMONS 
  val evaluation: EVAL
  import commons._
  import evaluation._

  private def pairMatchingRank(cards: Seq[Card]): Boolean = cards.length == 2 && countRank(cards).values.toSeq.contains(2)
  private def pairOfAces(cards: Seq[Card]): Boolean = pairMatchingRank(cards) && cards.head.rank == Ace

  // Only players can split on first play (only 2 cards) when both cards have same value
  // Tens, Jacks, Queens, and Kings are all considered the same value, meaning that player can split on Ten and Queen, etc... 
  // Dealers cannot split, but this function doesn't check for this
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

  def handCompleted(hand: Hand): Boolean = hand.wins.isDefined
  def handsCompleted(player: BlackjackPlayerState) = player.handsAndBets.count(h => handCompleted(h)) == player.hands.length
  def isFirstRound(hand: Hand): Boolean = hand.wins.isEmpty && hand.hand.length == 2

  def isTimeToPlay(game: BlackjackGameState): Boolean = {
    if (game.players == Nil)
      throw new IllegalArgumentException("cannot determine whether isTimeToPlay when there are no players")
    (for {
      player <- game.players
    } yield hasPlayerBet(game, player.id))
      .count(p => p) == game.players.length && 
      (game.players.flatMap(_.handsAndBets.map(h => h.wins)).count(w => w.isDefined) != game.players.length)
  }

  // TODO: test
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
    if (game.currentHand().length < 2) {
      throw new IllegalArgumentException(s"Cannot play current hand because current hand length [${game.currentHand().length}] is less than length 2")
    }
    if (game.dealerHand.hand.length != 2) {
      throw new IllegalArgumentException(s"Cannot play current hand because dealer's hand length [${game.dealerHand.hand.length}] is not length 2")
    }
    if (!isTimeToPlay(game)) {
      throw new IllegalArgumentException(s"Cannot play current hand because it's not currently time to play hand")
    }
    val canDoubleDown: Boolean = game.currentHand().length == 2
    // we only care about this current player's actions
    val previousActions: Seq[Action[BlackjackAction]] = game.history.filter(_.playerId == game.currentPlayer().id)
    // current player's split count from history
    val splitCount: Int = previousActions.count(a => a.action == Split)
    // current player's aces split count from history
    val acesSplitCount: Int = previousActions.count(a => pairOfAces(a.beforeCards) && a.action == Split) 
    val eligibleToSplit: Boolean = canSplit(game.currentHand(), game.options, splitCount, acesSplitCount)
    
    // player's turn: based on player's cards and dealers face up card, decide which action to take
    // looking at cards in reverse order so aces are at head, and changed to list in order to pattern match on head :: tail
    val highestRank: Rank = game.currentHand().sorted.reverse.head.rank
    val totalScore: Int = eval(game.currentHand())
    val tailScore: Int = eval(game.currentHand().tail)
    val dealerFaceUpRank: Rank = game.dealerHand.hand.head.rank
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

  // TODO: test
  // current player plays current hand
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
    if (game.currentHand().length < 2) {
      throw new IllegalArgumentException(s"Cannot play current hand because current hand length [${game.currentHand().length}] is less than length 2")
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
    
    val (updatedCurrentHand, splitHand): (Hand, Option[Hand]) = outcomeHands.length match {
      case 1 => (outcomeHands.head, None)
      case _ => (outcomeHands.head, Some(outcomeHands.tail.head))
    }
    var updatedHands: Seq[Hand] = 
      game.currentPlayer().handsAndBets.map { hand => hand.hand match {
        case cs if (cs == game.currentHand()) => updatedCurrentHand
        case _ => hand
      }
    }
    updatedHands = splitHand match {
      case Some(h) => updatedHands ++ Seq(h) // split occurred, add new hand
      case None => updatedHands 
    }
    game.copy(deck = updatedDeck, history = game.history ++ newHistory, players = for (p <- game.players) yield {
      if (p == game.currentPlayer())
        p.copy(handsAndBets = updatedHands)
      else p 
    })
  }

  // TODO: implement 
  // returns updated cards (seq of hands to account for Splits), updated deck, and new history
  def performPlayAction(
    playerId: String, 
    action: BlackjackAction, 
    cards: Seq[Card], 
    deck: Deck): (Seq[Hand], Deck, Seq[Action[BlackjackAction]]) = action match {
      case Hit => ???
      case Stand => ???
      case DoubleDown => ???
      case Split => ???
      case Surrender => ???
      case a => throw new IllegalArgumentException(s"Unexpected BlackjackAction [$a], at this phase the only expected actions are [Hit, Stand, DoubleDown, Split, Surrender]")
    }
}