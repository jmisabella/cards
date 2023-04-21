package cards.behaviors.controller

import cards.classes.state.{ PlayerState, GameState }
import cards.classes.actions.Action

// P is concrete PlayerState
// A is concrete Action enumeration value
// S is concrete GameState
trait Controller[P <: PlayerState, A <: Enumeration#Value, S <: GameState[P, A]] {

  // by default, don't print anything; subclasses should override next(S, Int) and next(S) to specify a different function
  private val doNothing = (a: Action[A]) => "" 

  // whether it's time to purge the history
  protected def purgeHistory(previous: S, current: S): Boolean = previous.round != current.round
  
  def next(game: S, iterations: Int, purgeHistoryAfterRound: Boolean): S = next(game, iterations, purgeHistoryAfterRound, doNothing) 
  
  def next(game: S): S = next(game, 1, true, doNothing)

  // serialize is an optional function which converts a game action to text
  def next(game: S, iterations: Int = 1, purgeHistoryAfterRound: Boolean = true, serialize: Action[A] => String): S

  private def safelyPlayNMoves(game: S, iterations: Int): S = {
    if (iterations <= 0) {
      throw new IllegalArgumentException(s"Invalid iterations [$iterations]; must be positive")
    }
    var maxIterations: Int = iterations
    var finished: Boolean = false
    var finalState: S = game
    while (!finished) {
      try { finalState = next(game, maxIterations, false); finished = true } catch {
        case _: Exception => {
          maxIterations -= 1
          finished = false
        }
      }
    }
    return finalState
  }

  def play(game: S, maxIterations: Int): S = {
    if (game.players.length == 0) {
      throw new IllegalArgumentException("Cannot play when there are no players")
    }
    val batchSize: Int = if (maxIterations < 500) maxIterations else 500
    var remaining = maxIterations
    var batch: Int = batchSize 
    var state: S = game 
    while (remaining > 0) {
      state = safelyPlayNMoves(state, batch)
      remaining = remaining - batch
      batch = if (remaining < batch) remaining else batch
    }
    state
  }
}
