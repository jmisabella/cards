package cards.modules

import cards.behaviors.Commons
import cards.behaviors.evaluation.{ BlackjackHandEvaluation, ThirtyOneHandEvaluation }
import cards.behaviors.betting.BlackjackBetting
import cards.behaviors.controller.{ BlackjackController, ThirtyOneController }
import cards.behaviors.play.BlackjackPlay
import cards.behaviors.Textualization
import cards.classes.actions.{ Action, BlackjackAction, ThirtyOneAction }
import cards.classes.actions.BlackjackAction._
import cards.classes.actions.ThirtyOneAction._
import cards.classes.state.{ BlackjackPlayerState, BlackjackGameState, ThirtyOnePlayerState, ThirtyOneGameState }

private [modules] case object _blackjackBetting extends BlackjackBetting {
  override type EVAL = BlackjackHandEvaluation
  override val evaluation = _blackjackEval
}

private [modules] case object _commons extends Commons
private [modules] case object _blackjackEval extends BlackjackHandEvaluation {
  override type C = Commons
  override val commons = _commons
}
private [modules] case object _thirtyoneEval extends ThirtyOneHandEvaluation {
  override type C = Commons
  override val commons = _commons
}
private [modules] case object _blackjackPlay extends BlackjackPlay {
  override type COMMONS = Commons
  override val commons = _commons
  override type EVAL = BlackjackHandEvaluation
  override val evaluation: EVAL = _blackjackEval
}

case object BlackjackText extends BlackjackController with Textualization {
  override type BETTING = BlackjackBetting
  override type PLAY = BlackjackPlay
  override val betting = _blackjackBetting
  override val play = _blackjackPlay

  private val f: Action[BlackjackAction] => String = words (_: Action[BlackjackAction])
  override def next(game: BlackjackGameState, iterations: Int): BlackjackGameState = super.next(game, iterations, f) 
  override def next(game: BlackjackGameState): BlackjackGameState = super.next(game, 1, f) 
}

case object Blackjack extends BlackjackController {
  override type BETTING = BlackjackBetting
  override type PLAY = BlackjackPlay
  override val betting = _blackjackBetting
  override val play = _blackjackPlay
}

case object ThirtyOneText extends ThirtyOneController with Textualization {
  override type EVAL = ThirtyOneHandEvaluation
  override val evaluation = _thirtyoneEval

  private val f: Action[ThirtyOneAction] => String = words (_: Action[ThirtyOneAction])
  override def next(game: ThirtyOneGameState, iterations: Int): ThirtyOneGameState = super.next(game, iterations, f) 
  override def next(game: ThirtyOneGameState): ThirtyOneGameState = super.next(game, 1, f) 
}

