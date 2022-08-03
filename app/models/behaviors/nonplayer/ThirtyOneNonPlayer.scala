package cards.models.behaviors.nonplayer

import cards.models.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.models.classes.state.{ ThirtyOnePlayerState, ThirtyOneGameState }
import cards.models.classes.{ Card, Deck }
import cards.models.classes.actions.{ Action, ThirtyOneAction }
import cards.models.classes.actions.ThirtyOneAction._

trait ThirtyOneNonPlayer {
  type EVAL <: ThirtyOneHandEvaluation 
  val evaluation: EVAL

  private def choose(cs1: Seq[Card], cs2: Seq[Card]): Seq[Card] = evaluation.preference(cs1, cs2) match {
    // TODO: test case (illegal state) when hands match 
    case None => throw new IllegalStateException(s"No rules defined for when hands perfectly match in ranks with suit groupings: hand 1 [${cs1.sorted}] hand 2 [${cs2.sorted}]")
    case Some(cs) => cs
  }

  // TODO: test
  def next[A <: Enumeration#Value](gameState: ThirtyOneGameState): ThirtyOneGameState = {
    if (gameState.deck.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because deck is empty")
    }
    if (gameState.discardPile.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because discard pile is empty")
    }
    if (gameState.winningPlayerId.isDefined) {
      throw new IllegalStateException(
        s"Cannot get next because a winning player already exists: winning player id [${gameState.winningPlayerId.get}], game state [$gameState]")
    }
    val currentPlayer: ThirtyOnePlayerState = gameState.currentPlayer()
    val completed: Boolean = gameState.knockedPlayerId.getOrElse("") == currentPlayer.id || gameState.players.count(p => evaluation.eval(p.hand) == 32) > 0
    if (completed) {
      // round is completed, determine winner 
      val winningHand: Seq[Card] = gameState.players.map(_.hand).foldLeft(Nil: Seq[Card]) { (acc, a) => choose(acc, a) }
      val winner: String = gameState.players.filter(p => p.hand.sorted == winningHand.sorted).map(_.id).head
      return gameState.copy(winningPlayerId = Some(winner))
    }
    // round is not yet completed, play next turn
    val currentHand: Seq[Card] = currentPlayer.hand
    val currentScore: Int = evaluation.eval(currentHand)
    val nextPlayer: ThirtyOnePlayerState = gameState.players(gameState.nextPlayerIndex())
    // if knocking, should make sure that next player would not get a thirty one automatic win or a higher score from drawing from discard pile 
    val nextPlayerDrawDiscardPotentialScore: Int = evaluation.permutationsAndScores(Seq(gameState.discardPile.head) ++ nextPlayer.suspectedCards, 3).maxBy(_._2)._2

    // early knock logic
    // TODO: test early knock logic
    if (!gameState.knockedPlayerId.isDefined && gameState.history.length == 0 && currentScore > 17) {
      // if, during the very first turn, hand is good enough then knock right away
      return gameState.copy(
        knockedPlayerId = Some(currentPlayer.id), 
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ Seq(Action(currentPlayer.id, Knock)))
    }
    if (!gameState.knockedPlayerId.isDefined && gameState.history.length < gameState.players.length && 
      currentScore > 21 && 
      nextPlayerDrawDiscardPotentialScore != 32 && 
      nextPlayerDrawDiscardPotentialScore < currentScore) {
      // if, during the first round, hand is good enough then knock
      return gameState.copy(
        knockedPlayerId = Some(currentPlayer.id), 
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ Seq(Action(currentPlayer.id, Knock)))
    }

    // draw discard logic
    // TODO: test draw discard logic
    val drawDiscardPermutationsAndScores: Seq[(Seq[Card], Int)] = evaluation.permutationsAndScores(cards = currentHand ++ Seq(gameState.discardPile.head), n = 3)
    val drawDiscardPileHand: Seq[Card] = drawDiscardPermutationsAndScores.maxBy(_._2)._1
    val drawDiscardScore: Int =  evaluation.eval(drawDiscardPileHand)
    val drawDiscardPileDiscardedCard: Card = (currentHand ++ Seq(gameState.discardPile.head)).diff(drawDiscardPileHand).head
    if (drawDiscardScore == 32) { // draw from discard if it leads to 31 (instant win)
      return gameState.copy(
        winningPlayerId = Some(currentPlayer.id),
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ 
          Seq(Action(currentPlayer.id, DrawFromDiscard, Seq(gameState.discardPile.head)), Action(currentPlayer.id, Discard, Seq(drawDiscardPileDiscardedCard))), 
        players = gameState.updatedHandAndSuspectedCards(updatedHand = drawDiscardPileHand, discarded = Seq(drawDiscardPileDiscardedCard), publiclyViewedNewCards = Seq(gameState.discardPile.head)),
        discardPile = gameState.discardPile.tail)
    }
    val drawDiscardNextPlayerPotentialScore: Int = evaluation.permutationsAndScores(Seq(drawDiscardPileDiscardedCard) ++ nextPlayer.suspectedCards, 3).maxBy(_._2)._2
    if (drawDiscardNextPlayerPotentialScore < 32 && drawDiscardScore > currentScore && (drawDiscardScore - currentScore > 8 || drawDiscardScore >= 30)) {
      return gameState.copy(
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ 
          Seq(Action(currentPlayer.id, DrawFromDiscard, Seq(gameState.discardPile.head)), Action(currentPlayer.id, Discard, Seq(drawDiscardPileDiscardedCard))), 
        players = gameState.updatedHandAndSuspectedCards(updatedHand = drawDiscardPileHand, discarded = Seq(drawDiscardPileDiscardedCard), publiclyViewedNewCards = Seq(gameState.discardPile.head)),
        discardPile = gameState.discardPile.tail)
    }
    
    // knock logic 
    // TODO: test knock logic 
    if (!gameState.knockedPlayerId.isDefined &&
      currentScore >= 30 && 
      nextPlayerDrawDiscardPotentialScore != 32 && 
      nextPlayerDrawDiscardPotentialScore < currentScore) {
      // if, during the first round, hand is good enough then knock
      return gameState.copy(
        knockedPlayerId = Some(currentPlayer.id), 
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ Seq(Action(currentPlayer.id, Knock)))
    }

    // draw logic
    // TODO: test draw logic
    val (drawnCard, updatedDeck): (Seq[Card], Deck) = gameState.deck.deal()
    val drawCardPermutationsAndScores: Seq[(Seq[Card], Int)] = evaluation.permutationsAndScores(cards = currentHand ++ drawnCard, n = 3)
    // determine if next player would have a better or even winning hand if he or she picks up any of this player's discarded cards
    val nextPlayerPotentialScores: Map[Card, Int] = 
      (currentHand ++ drawnCard)
        .map { c =>  
          c -> evaluation
            .permutationsAndScores(Seq(c) ++ nextPlayer.suspectedCards, n = 3)
            .maxBy(_._2)
            ._2 
        }
        .toMap

    val discardsToAvoid: Seq[Card] = nextPlayerPotentialScores.filter(_._2 >= 32).toSeq.map(_._1)
    val safePermutationsAndScores: Seq[(Seq[Card], Int)] = drawCardPermutationsAndScores.filter(_._1.intersect(discardsToAvoid).nonEmpty)
    val drawHand: Seq[Card] = safePermutationsAndScores.maxBy(_._2)._1
    val drawHandScore: Int = evaluation.eval(drawHand)
    val discardedCard: Card = (currentHand ++ drawnCard).diff(drawHand).head
    return gameState.copy(
      currentPlayerIndex = Some(gameState.nextPlayerIndex()),
      history = gameState.history ++ 
        Seq(Action(currentPlayer.id, Draw, drawnCard), Action(currentPlayer.id, Discard, Seq(discardedCard))), 
      players = gameState.updatedHandAndSuspectedCards(updatedHand = drawHand, discarded = Seq(discardedCard)),
      deck = updatedDeck,
      discardPile = Seq(discardedCard) ++ gameState.discardPile)
  }

}
