package cards.models.behaviors.play

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }
import cards.models.classes.hand.Hand
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._

trait BlackjackPlay {
  
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

  def playFirstHand(game: BlackjackGameState): BlackjackGameState = {
    if (game.players == Nil) {
      throw new IllegalArgumentException("Cannot play first hand because there are no players") 
    }
    if (game.currentPlayerIndex.isEmpty) {
      throw new IllegalArgumentException("Cannot play first hand because current player is not specified")
    }
    if (game.currentHandIndex.isEmpty) {
      throw new IllegalArgumentException("Cannot play first hand because current hand is not specified")
    }
    if (game.currentHand().length != 2) {
      throw new IllegalArgumentException(s"Cannot play first hand because current hand length [${game.currentHand().length}] is not length 2")
    }
    if (game.dealerHand.hand.length != 2) {
      throw new IllegalArgumentException(s"Cannot play first hand because dealer's hand length [${game.dealerHand.hand.length}] is not length 2")
    }
    // TODO: test edge cases above
    val action: Action[BlackjackAction] = (game.currentHand().sorted, game.dealerHand.hand) match {
      // TODO: implement 
      case (_, _) => ???
    }
    action match {
      // TODO: implement 
      case (_) => ???
    } 
    ???
  } 
}