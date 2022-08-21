package cards.models.classes.state
import cards.models.classes.Deck
import cards.models.classes.actions.Action

trait PlayerState {
  val id: String
}

trait GameState[A <: PlayerState, B <: Enumeration#Value] {
  val players: Seq[A]
  val currentPlayerIndex: Option[Int]
  val history: Seq[Action[B]]
  val deck: Deck

    def currentPlayer(): A = (players, currentPlayerIndex) match {
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

}