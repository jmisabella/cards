package cards.behaviors.controller

import cards.classes.state.{ PlayerState, GameState }
import cards.classes.actions.Action

// P is concrete PlayerState
// A is concrete Action enumeration value
// S is concrete GameState
trait Controller[P <: PlayerState, A <: Enumeration#Value, S <: GameState[P, A]] {
  
  private val doNothing = (a: Action[A]) => "" 

  protected def purgeHistory(previous: S, current: S): Boolean = previous.round != current.round
  
  def next(game: S, iterations: Int): S = next(game, iterations, doNothing) 
  
  def next(game: S): S = next(game, 1, doNothing)

  // serialize is an optional function which converts a game action to text
  def next(game: S, iterations: Int = 1, serialize: Action[A] => String): S

}
