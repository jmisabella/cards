package cards.behaviors.controller

import cards.classes.state.{ PlayerState, GameState }
import cards.classes.actions.Action

trait Controller[A <: PlayerState, B <: Enumeration#Value, C <: GameState[A, B]] {
  
  private val doNothing = (a: Action[B]) => "" 

  def purgeHistory(previous: C, current: C): Boolean = previous.round != current.round
  
  def next(game: C, iterations: Int): C = next(game, iterations, doNothing) 
  
  def next(game: C): C = next(game, 1, doNothing)

  // serialize is an optional function which converts a game action to text
  def next(game: C, iterations: Int = 1, serialize: Action[B] => String): C

}
