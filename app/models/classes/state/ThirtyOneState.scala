package cards.models.classes.state

import cards.models.classes.state.{ PlayerState, GameState }
import cards.models.classes.{ Card, Suit, Deck }
import cards.models.classes.Suit._
import cards.models.classes.DeckType._
import cards.models.classes.actions.{ Action, ThirtyOneAction }
import cards.models.classes.actions.ThirtyOneAction._

case class ThirtyOnePlayerState(
  override val id: String, 
  tokens: Int = 4, // simulates the idea of player having 3 tokens and then 1 free "on-the-house" play
  hand: Seq[Card] = Nil,
  suspectedSuitChange: Boolean = false,
  suspectedCards: Seq[Card] = Nil) extends PlayerState

case class ThirtyOneGameState(
  override val players: Seq[ThirtyOnePlayerState],
  override val currentPlayerIndex: Option[Int] = None,
  override val history: Seq[Action[ThirtyOneAction]] = Nil,
  override val deck: Deck = Deck(JokersExcluded),
  discardPile: Seq[Card] = Nil,
  pot: Int = 0,
  knockedPlayerId: Option[String] = None,
  winningPlayerId: Option[String] = None) extends GameState[ThirtyOnePlayerState, ThirtyOneAction] {

    require(players.length <= 7, s"Cannot have more than 7 players in the game of Thirty-One. There are currently [${players.length}] players")

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