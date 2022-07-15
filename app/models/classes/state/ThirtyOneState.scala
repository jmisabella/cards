package cards.models.classes.state

import cards.models.classes.{ Card, Suit, Deck }
import cards.models.classes.Suit._
import cards.models.classes.DeckType._
import cards.models.classes.actions.{ Action, ThirtyOneAction }
import cards.models.classes.actions.ThirtyOneAction._

case class ThirtyOnePlayerState(
  id: String, 
  tokens: Int, 
  hand: Seq[Card] = Nil, 
  suspectedSuit: Option[Suit] = None, 
  suspectedCards: Seq[Card] = Nil)

case class ThirtyOneGameState(
  players: Seq[ThirtyOnePlayerState], 
  deck: Deck = Deck(JokersExcluded), 
  discardPile: Seq[Card] = Nil, 
  pot: Int = 0, 
  history: Seq[Action[ThirtyOneAction]] = Nil)