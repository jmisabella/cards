package cards.classes.state

import cards.classes.Deck
import cards.classes.actions.Action

trait PlayerState {
  val id: String
}

trait GameState[A <: PlayerState, B <: Enumeration#Value] {
  val players: Seq[A]
  val currentPlayerIndex: Option[Int]
  val history: Seq[Action[B]]
  val deck: Deck
    
    def currentPlayer(): A = current(players, currentPlayerIndex)
    def nextPlayerIndex(): Int = nextIndex(players, currentPlayerIndex)

    protected def current[X](list: Seq[X], currentIndex: Option[Int]): X = (list, currentIndex) match {
      case (Nil, _) => throw new IllegalStateException(s"Cannot determine current element when no elements exist")
      case (xs, None) => throw new IllegalStateException(s"Cannot determine current element when currentIndex is empty")
      case (xs, Some(i)) if (i >= xs.length) => 
        throw new IllegalStateException(s"Cannot determine current element because currentIndex [$i] is outside of range list length [${list.length}]")
      case (xs, Some(i)) => xs(i)  
    }

    protected def nextIndex[X](list: Seq[X], currentIndex: Option[Int]): Int = (list, currentIndex) match {
      case (Nil, _) => throw new IllegalStateException(s"Cannot determine next index when no elements exist")
      case (xs, None) => throw new IllegalStateException(s"Cannot determine next index when currentIndex is empty")
      case (xs, Some(i)) => (i + 1) % xs.length
    } 
}