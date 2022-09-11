package cards.models.behaviors.play

import cards.models.behaviors.Commons
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import cards.models.classes.options.BlackjackOptions

trait BlackjackPlay {
  type C <: Commons
  val commons: C 
  import commons._

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
    // TODO: test edge cases above
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
    val action: Action[BlackjackAction] = (game.currentHand().sorted.reverse.toList, game.dealerHand.hand.head, canDoubleDown, eligibleToSplit) match {
      // TODO: implement 
      case (_, _, _, _) => ???
    }
    // perform the action
    action match {
      // TODO: implement 
      case (_) => ???
    } 
    ???
  } 
}