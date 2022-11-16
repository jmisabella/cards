package cards.modules

import cards.behaviors.Commons
import cards.behaviors.evaluation.BlackjackHandEvaluation
import cards.behaviors.betting.BlackjackBetting
import cards.behaviors.controller.BlackjackController
import cards.behaviors.play.BlackjackPlay
import cards.behaviors.Textualization
import cards.classes.actions.{ Action, BlackjackAction }
import cards.classes.actions.BlackjackAction._
import cards.classes.state.{ BlackjackPlayerState, BlackjackGameState }

private [modules] case object _betting extends BlackjackBetting {
  override type EVAL = BlackjackHandEvaluation
  override val evaluation = _evaluation
}

private [modules] case object _commons extends Commons
private [modules] case object _evaluation extends BlackjackHandEvaluation {
  override type C = Commons
  override val commons = _commons
}
private [modules] case object _play extends BlackjackPlay {
  override type COMMONS = Commons
  override val commons = _commons
  override type EVAL = BlackjackHandEvaluation
  override val evaluation: EVAL = _evaluation 
}

case object BlackjackText extends BlackjackController with Textualization {
  override type BETTING = BlackjackBetting
  override type PLAY = BlackjackPlay
  override val betting = _betting
  override val play = _play

  private val f: Action[BlackjackAction] => String = words (_: Action[BlackjackAction])
  override def next(game: BlackjackGameState, iterations: Int): BlackjackGameState = super.next(game, iterations, f) 
  override def next(game: BlackjackGameState): BlackjackGameState = super.next(game, 1, f) 
}
