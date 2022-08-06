package cards.models.behaviors.nonplayer

import cards.models.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.models.classes.state.{ ThirtyOnePlayerState, ThirtyOneGameState }
import cards.models.classes.{ Card, Deck }
import cards.models.classes.actions.{ Action, ThirtyOneAction }
import cards.models.classes.actions.ThirtyOneAction._

// Game play follows West Lansing Cut Throat rules
trait ThirtyOneNonPlayer { 
  type EVAL <: ThirtyOneHandEvaluation 
  val evaluation: EVAL

  private def highestHand(cs1: Seq[Card], cs2: Seq[Card]): Seq[Card] = evaluation.preference(cs1, cs2) match {
    // TODO: test case (illegal state) when hands match 
    case None => throw new IllegalStateException(s"No rules defined for when hands perfectly match in ranks with suit groupings: hand 1 [${cs1.sorted}] hand 2 [${cs2.sorted}]")
    case Some(cs) => cs.sorted
  }
  // in case of tie, both lowest hands pay
  // TODO: test 
  private def lowestHands(hands: Seq[Seq[Card]]): Seq[Seq[Card]] = {
    val lowest: Int = hands.map(cs => evaluation.eval(cs)).sorted.head
    hands.filter(cs => evaluation.eval(cs) == lowest).map(_.sorted)
  } 

  def next(gameState: ThirtyOneGameState): ThirtyOneGameState = {
    // TODO: test when more than 7 players
    if (gameState.players.length > 7) {
      throw new IllegalStateException(
        s"Cannot get next because [${gameState.players.length}] players exceeds the seven player maximum")
    } 
    if (gameState.deck.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because deck is empty")
    }
    if (gameState.discardPile.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because discard pile is empty")
    }
    // TODO: test 
    if (gameState.winningPlayerId.isDefined) {
      // lowest hand plays 1 token; knocker pays double if knocker has lowest hand... 
      // update players' tokens and state's history, also remove losing (broke) players from the game
      val losers: Seq[String] = gameState.players.filter(p => lowestHands(gameState.players.map(_.hand)).contains(p.hand.sorted)).map(_.id)
      val loserDebts: Map[String, Int] = (for (p <- losers) yield if (gameState.knockedPlayerId.getOrElse("") == p) p -> 2 else p -> 1).toMap
      val paymentHistory: Seq[Action[ThirtyOneAction]] = (for ((player, debt) <- loserDebts) yield Action(player, Pay, Nil, actionTokens = debt)).toSeq
      val updatedPlayers: Seq[ThirtyOnePlayerState] = gameState.updatedTokens(loserDebts)
      val removedPlayers: Seq[String] = updatedPlayers.filter(p => p.tokens <= 0).map(_.id)
      val lostPlayerHistory: Seq[Action[ThirtyOneAction]] = removedPlayers.map(p => Action(p, Out))
      return gameState.copy(history = gameState.history ++ paymentHistory ++ lostPlayerHistory, players = updatedPlayers.filter(p => !removedPlayers.contains(p.id)))
    } 
    val currentPlayer: ThirtyOnePlayerState = gameState.currentPlayer()
    val completed: Boolean = gameState.knockedPlayerId.getOrElse("") == currentPlayer.id || gameState.players.count(p => evaluation.eval(p.hand) == 32) > 0
    if (completed) {
      // round is completed, determine winner 
      val winningHand: Seq[Card] = gameState.players.map(_.hand).foldLeft(Nil: Seq[Card]) { (acc, a) => highestHand(acc, a) }
      val winner: String = gameState.players.filter(p => p.hand.sorted == winningHand.sorted).map(_.id).head
      return gameState.copy(winningPlayerId = Some(winner))
    }
    // round is not yet completed, play next turn
    val currentHand: Seq[Card] = currentPlayer.hand
    val currentScore: Int = evaluation.eval(currentHand)
    val nextPlayer: ThirtyOnePlayerState = gameState.players(gameState.nextPlayerIndex())
    val suspected: Seq[Card] = nextPlayer.suspectedCards
    
    // draw discard logic
    val drawDiscardPermutationsAndScores: Seq[(Seq[Card], Int)] = evaluation.permutationsAndScores(cards = currentHand ++ Seq(gameState.discardPile.head), n = 3)
    val drawDiscardPileHand: Seq[Card] = drawDiscardPermutationsAndScores.length match {
      case 0 => Nil
      case _ => drawDiscardPermutationsAndScores.maxBy(_._2)._1
    }
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
    val drawDiscardNextPlayerPermutations = evaluation.permutationsAndScores(Seq(drawDiscardPileDiscardedCard) ++ suspected, suspected.length)
    val drawDiscardNextPlayerPotentialScore: Int = drawDiscardNextPlayerPermutations.length match {
      case 0 => 0
      case _ => drawDiscardNextPlayerPermutations.maxBy(_._2)._2
    }
    if (drawDiscardNextPlayerPotentialScore < 32 && drawDiscardScore > currentScore && (drawDiscardScore - currentScore >= 7 || drawDiscardScore >= 30)) {
      return gameState.copy(
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ 
          Seq(Action(currentPlayer.id, DrawFromDiscard, Seq(gameState.discardPile.head)), Action(currentPlayer.id, Discard, Seq(drawDiscardPileDiscardedCard))), 
        players = gameState.updatedHandAndSuspectedCards(updatedHand = drawDiscardPileHand, discarded = Seq(drawDiscardPileDiscardedCard), publiclyViewedNewCards = Seq(gameState.discardPile.head)),
        discardPile = gameState.discardPile.tail)
    }
    
    // early knock logic
    if (!gameState.knockedPlayerId.isDefined && gameState.history.length == 0 && currentScore > 17) {
      // if, during the very first turn, hand is good enough then knock right away
      return gameState.copy(
        knockedPlayerId = Some(currentPlayer.id), 
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ Seq(Action(currentPlayer.id, Knock)))
    }
    // if knocking, should make sure that next player would not get a thirty one automatic win or a higher score from drawing from discard pile
    val nextPlayerDrawDiscardPotentialScore: Int = evaluation.permutationsAndScores(Seq(gameState.discardPile.head) ++ suspected, suspected.length).maxBy(_._2)._2
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
    
    // regular knock logic 
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

    // draw logic: either draw from stock deck or draw from discard pile
    // TODO: test draw logic
    val (drawnCard, updatedDeck): (Seq[Card], Deck) = gameState.deck.deal()
    val drawCardPermutationsAndScores: Seq[(Seq[Card], Int)] = evaluation.permutationsAndScores(cards = currentHand ++ drawnCard, n = 3)
    // determine if next player would have a better or even winning hand if he or she picks up any of this player's discarded cards
    val nextPlayerPotentialScores: Map[Card, Int] = 
      (currentHand ++ drawnCard)
        .map { c =>  
          c -> evaluation
            .permutationsAndScores(Seq(c) ++ suspected, n = suspected.length)
            .maxBy(_._2)
            ._2 
        }
        .toMap

    val discardsToAvoid: Seq[Card] = nextPlayerPotentialScores.filter(_._2 >= 32).toSeq.map(_._1)
    val safePermutationsAndScores: Seq[(Seq[Card], Int)] = drawCardPermutationsAndScores.filter(_._1.intersect(discardsToAvoid).nonEmpty)
    val drawHand: Seq[Card] = safePermutationsAndScores.length match {
      case 0 => Nil
      case _ => safePermutationsAndScores.maxBy(_._2)._1
    }
    val drawHandScore: Int = evaluation.eval(drawHand)
    val discardedCard: Card = (currentHand ++ drawnCard).diff(drawHand).head
    return gameState.copy(
      currentPlayerIndex = Some(gameState.nextPlayerIndex()),
      history = gameState.history ++ 
        Seq(Action(currentPlayer.id, DrawFromStock, drawnCard), Action(currentPlayer.id, Discard, Seq(discardedCard))), 
      players = gameState.updatedHandAndSuspectedCards(updatedHand = drawHand, discarded = Seq(discardedCard)),
      deck = updatedDeck,
      discardPile = Seq(discardedCard) ++ gameState.discardPile)
  }

}
