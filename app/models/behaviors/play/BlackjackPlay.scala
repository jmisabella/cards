package cards.models.behaviors.play

import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.{ Card, Rank, Suit, Deck }

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

  def isTimeToPlay(game: BlackjackGameState): Boolean = {
    if (game.players == Nil)
      throw new IllegalArgumentException("cannot determine whether isTimeToPlay when there are no players")
    (for {
      player <- game.players
    } yield hasPlayerBet(game, player.id))
      .count(p => p) == game.players.length && 
      (game.players.flatMap(_.handsAndBets.map(h => h.wins)).count(w => w.isDefined) != game.players.length)
  }

}