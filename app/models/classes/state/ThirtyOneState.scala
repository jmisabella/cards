package cards.models.classes.state

import cards.models.classes.{ Card, Suit, Deck }
import cards.models.classes.Suit._
import cards.models.classes.DeckType._
import cards.models.classes.actions.{ Action, ThirtyOneAction }
import cards.models.classes.actions.ThirtyOneAction._

case class ThirtyOnePlayerState(
  id: String, 
  tokens: Int = 3,
  hand: Seq[Card] = Nil,
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

    // TODO: test
    def currentPlayer(): ThirtyOnePlayerState = (players, currentPlayerIndex) match {
      case (Nil, _) => throw new IllegalStateException(s"Cannot determine current player when no players exist")
      case (ps, None) => throw new IllegalStateException(s"Cannot determine current player when currentPlayerIndex is empty")
      case (ps, Some(i)) if (i >= ps.length) => 
        throw new IllegalStateException(s"Cannot determine current player because currentPlayerIndex [$i] is outside of range of players list length [${players.length}]")
      case (ps, Some(i)) => ps(i)  
    }

    // TODO: test
    def nextPlayerIndex(): Int = (players, currentPlayerIndex) match {
      case (Nil, _) => throw new IllegalStateException(s"Cannot determine next player when no players exist")
      case (ps, None) => 0 // no previous index set, so start with first player
      case (ps, Some(i)) => (i + 1) % ps.length
    }

    // // TODO: test
    // // yields an updated players list with the current player's cards reflecting specified updated cards
    // def updatedHand(updatedHand: Seq[Card]): Seq[ThirtyOnePlayerState] = {
    //   for (p <- players) yield { 
    //     if (p == currentPlayer()) 
    //       p.copy(hand = updatedHand) 
    //     else 
    //       p
    //   }   
    // }

    // // TODO: test
    // // yields an updated players list with the current player's suspected cards reflecting specified new and discarded cards
    // def updatedSuspectedCards(newCards: Seq[Card] = Nil, discarded: Seq[Card] = Nil): Seq[ThirtyOnePlayerState] = {
    //   for (p <- players) yield { 
    //     if (p == currentPlayer()) 
    //       p.copy(suspectedCards = p.suspectedCards.diff(discarded) ++ newCards) 
    //     else 
    //       p
    //   }   
    // }
    // def updatedSuspectedCards(newCard: Card, discard: Card): Seq[ThirtyOnePlayerState] = updatedSuspectedCards(newCards = Seq(newCard), discarded = Seq(discard))

    // TODO: test
    def updatedHandAndSuspectedCards(updatedHand: Seq[Card], discarded: Seq[Card], publiclyViewedNewCards: Seq[Card] = Nil): Seq[ThirtyOnePlayerState] = {
      for (p <- players) yield { 
        if (p == currentPlayer()) 
          p.copy(hand = updatedHand, suspectedCards = p.suspectedCards.diff(discarded) ++ publiclyViewedNewCards)
        else 
          p
      }   
    }
}