package cards.models.classes.state

import cards.models.classes.{ Card, Suit, Deck }
import cards.models.classes.Suit._
import cards.models.classes.DeckType._
import cards.models.classes.actions.{ Action, ThirtyOneAction }
import cards.models.classes.actions.ThirtyOneAction._

case class ThirtyOnePlayerState(
  id: String, 
  tokens: Int = 4, // simulates the idea of player having 3 tokens and then 1 free "on-the-house" play
  hand: Seq[Card] = Nil,
  suspectedSuitChange: Boolean = false,
  suspectedCards: Seq[Card] = Nil)

case class ThirtyOneGameState(
  players: Seq[ThirtyOnePlayerState],
  deck: Deck = Deck(JokersExcluded),
  discardPile: Seq[Card] = Nil,
  pot: Int = 0,
  currentPlayerIndex: Option[Int] = None,
  knockedPlayerId: Option[String] = None,
  winningPlayerId: Option[String] = None,
  history: Seq[Action[ThirtyOneAction]] = Nil) {

    require(players.length <= 7, s"Cannot have more than 7 players in the game of Thirty-One. There are currently [${players.length}] players")

    def currentPlayer(): ThirtyOnePlayerState = (players, currentPlayerIndex) match {
      case (Nil, _) => throw new IllegalStateException(s"Cannot determine current player when no players exist")
      case (ps, None) => throw new IllegalStateException(s"Cannot determine current player when currentPlayerIndex is empty")
      case (ps, Some(i)) if (i >= ps.length) => 
        throw new IllegalStateException(s"Cannot determine current player because currentPlayerIndex [$i] is outside of range of players list length [${players.length}]")
      case (ps, Some(i)) => ps(i)  
    }

    def nextPlayerIndex(): Int = (players, currentPlayerIndex) match {
      case (Nil, _) => throw new IllegalStateException(s"Cannot determine next player when no players exist")
      case (ps, None) => throw new IllegalStateException(s"Cannot determine next player when currentPlayerIndex is empty")
      case (ps, Some(i)) => (i + 1) % ps.length
    }

    // yields an updated players list with the current player's hand and suspected cards reflecting specified newly drawn and discarded cards
    def updatedHandAndSuspectedCards(updatedHand: Seq[Card], discarded: Seq[Card], suspectedSuitChange: Boolean = false, publiclyViewedNewCards: Seq[Card] = Nil): Seq[ThirtyOnePlayerState] = {
      for (p <- players) yield { 
        if (p == currentPlayer()) 
          p.copy(hand = updatedHand, suspectedSuitChange = suspectedSuitChange, suspectedCards = p.suspectedCards.diff(discarded) ++ publiclyViewedNewCards)
        else 
          p
      }   
    }

    def updatedTokens(loserDebts: Map[String, Int]): Seq[ThirtyOnePlayerState] = {
      for (p <- players) yield {
        if (loserDebts.keySet.contains(p.id))
          p.copy(tokens = p.tokens - loserDebts(p.id)) 
        else 
          p
      }
    }

}