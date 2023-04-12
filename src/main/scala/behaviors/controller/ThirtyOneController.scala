package cards.behaviors.controller

import cards.behaviors.evaluation.ThirtyOneHandEvaluation
import cards.classes.state.{ ThirtyOnePlayerState, ThirtyOneGameState }
import cards.classes.{ Card, Deck }
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.actions.{ Action, ThirtyOneAction }
import cards.classes.actions.ThirtyOneAction._

// Game play follows West Lansing Cut Throat rules
trait ThirtyOneController extends Controller[ThirtyOnePlayerState, ThirtyOneAction, ThirtyOneGameState] { 
  type EVAL <: ThirtyOneHandEvaluation 
  val evaluation: EVAL

  private def facedown(cards: Seq[Card]): Seq[Card] = cards.map(_ => Card(FaceDown, Unknown))

  private def highestHand(cs1: Seq[Card], cs2: Seq[Card]): Seq[Card] = evaluation.preference(cs1, cs2) match {
    case None => throw new IllegalStateException(s"No rules defined for when hands perfectly match in ranks with suit groupings: hand 1 [${cs1.sorted}] hand 2 [${cs2.sorted}]")
    case Some(cs) => cs.sorted
  }
  // in case of tie, both lowest hands pay
  private def lowestHands(hands: Seq[Seq[Card]]): Seq[Seq[Card]] = {
    val lowest: Long = hands.map(cs => evaluation.eval(cs)).sorted.head
    hands.filter(cs => evaluation.eval(cs) == lowest).map(_.sorted)
  } 
  private def suspectedSuitChange(playerId: String, state: ThirtyOneGameState): Boolean = {
    // discard history 
    val h: Seq[Action[ThirtyOneAction]] = state.history.filter(a => a.playerId == playerId && a.action == Discard && a.actionCards != Nil)
    h.length > 1 && Seq(h.reverse.head, h.reverse.tail.head).map(_.actionCards.head.suit).distinct.length == 1
  }
  private def suspectedSuitChange(playerId: String, lastDiscard: Card, state: ThirtyOneGameState): Boolean = {
    // discard history 
    val h: Seq[Action[ThirtyOneAction]] = state.history.filter(a => a.playerId == playerId && a.action == Discard && a.actionCards != Nil)
    h.length > 1 && (Seq(h.reverse.head).map(_.actionCards.head.suit) ++ Seq(lastDiscard.suit)).distinct.length == 1
  }

  //////////////////////////////

  // iterations: number of iterations to take (number of moves)
  // purgeHistory: whether to purge history after all players have completed the round
  // serialize is an optional function which converts a blackjack action to text
  override def next(game: ThirtyOneGameState, iterations: Int = 1, purgeHistoryAfterRound: Boolean = true, serialize: Action[ThirtyOneAction] => String): ThirtyOneGameState = {
    def turns(game: ThirtyOneGameState, iterations: Int): ThirtyOneGameState = {
      def turn(game: ThirtyOneGameState, serialize: Action[ThirtyOneAction] => String): ThirtyOneGameState = {
        val next = go(game)
        (purgeHistoryAfterRound, purgeHistory(game, next)) match {
          case (false, _) => next
          case (_, false) => next
          case (true, true) => {
            for (a <- next.history) {
              val printed: String = serialize(a)
              if (printed != "") {
                // print history before purging it
                println(printed)
              }
            }
            // purge history
            next.copy(history = Nil)
          }
        }
      } 
      var state = game
      for (i <- (0 to iterations)) {
        try {
          state = turn(state, serialize)
        } catch {
          case _: IllegalStateException => try {
            state = turn(state, serialize)
          } catch { 
            case _: IllegalStateException => state = turn(state, serialize)
          }
        }
      }
      state
    }
    turns(game, iterations)
  }

  //////////////////////////////

  override def next(gameState: ThirtyOneGameState): ThirtyOneGameState = {
    go(gameState)
  }

  private def go(gameState: ThirtyOneGameState): ThirtyOneGameState = {
    if (gameState.deck.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because deck is empty")
    }
    if (gameState.players.length == 0) {
      throw new IllegalStateException(
        s"Cannot get next because there are no players")
    }
    if (gameState.discardPile.length == 0) {
      // no cards yet in discard pile, so  deal 3
      val (discardPile, deck): (Seq[Card], Deck) = gameState.deck.deal(3)
      val history: Seq[Action[ThirtyOneAction]] = Seq(Action("Discard pile", IsDealt, Seq(discardPile.head)))
      return gameState.copy(discardPile = discardPile, deck = deck, currentPlayerIndex = Some(0), history = history)
    }
    if (gameState.discardPile.length >= (52 - gameState.players.length - 3)) {
      // if discard pile length is close to a certain amount, then return discard pile to deck (action Shuffle)
      val returnedCards: Seq[Card] = gameState.discardPile.tail.tail.tail // skip top 3 cards
      val discardPile: Seq[Card] = Seq(gameState.discardPile.head, gameState.discardPile.tail.head, gameState.discardPile.tail.tail.head)
      val history: Seq[Action[ThirtyOneAction]] = Seq(Action(gameState.currentPlayer().id, Shuffle))
      return gameState.copy(
        discardPile = discardPile, 
        deck = gameState.deck.copy(cards = gameState.deck.cards ++ returnedCards), 
        history = gameState.history ++ history)
    }

    if (gameState.players.count(p => p.hand.length == 0) == gameState.players.length) {
      // if players have no cards then deal cards
      var deck: Deck = gameState.deck
      var history: Seq[Action[ThirtyOneAction]] = Nil 
      val players: Seq[ThirtyOnePlayerState] = gameState.players.map { p =>
        val (dealt, updatedDeck): (Seq[Card], Deck) = deck.deal(3 - p.hand.length)
        deck = updatedDeck
        val actionCards: Seq[Card] = gameState.debug match {
          case true => dealt // if in debug mode, then show cards
          case false => facedown(dealt) // hide cards
        }
        history = history ++ Seq(Action(p.id, IsDealt, actionCards))
        p.copy(hand = p.hand ++ dealt)
      }
      history = history ++ Seq(Action("Discard pile", IsDealt, Seq(gameState.discardPile.head)))
      return gameState.copy(deck = deck, history = gameState.history ++ history, players = players)
    }

    if (gameState.winningPlayerId.isDefined) {
      // if score of 31 is reached and nobody's knocked yet, then all other non-31 players should pay 1 token
      // otherwise the lowest hand(s) pays 1 token; knocker pays double if knocker has lowest hand... 
      // any losers with no remaining tokens are removed from the game
      val losers: Seq[String] = (gameState.knockedPlayerId, gameState.players.count(p => evaluation.eval(p.hand) >= 32) > 0) match {
        // nobody's knocked and a player got 31 (blitz), which means that all other players must pay 1 token 
        case (None, true) => gameState.players.filter(p => evaluation.eval(p.hand) < 32).map(_.id)
        // else nobody's blitzed with a 31, so lowest hand(s) must pay; debt is 1 unless the loser knocked 
        case (_, _) => gameState.players.filter(p => lowestHands(gameState.players.map(_.hand)).contains(p.hand.sorted)).map(_.id)
      }
      val loserDebts: Map[String, Int] = (for (p <- losers) yield if (gameState.knockedPlayerId.getOrElse("") == p) p -> 2 else p -> 1).toMap

      val updatedPot: Int = gameState.pot + loserDebts.values.toList.foldLeft(0)(_ + _)
      val paymentHistory: Seq[Action[ThirtyOneAction]] = (for ((player, debt) <- loserDebts) yield Action(player, Pay, Nil, actionTokens = Some(debt))).toSeq
      var updatedPlayers: Seq[ThirtyOnePlayerState] = gameState.updatedTokens(loserDebts)
      val removedPlayers: Seq[String] = updatedPlayers.filter(p => p.tokens <= 0).map(_.id)
      val lostPlayerHistory: Seq[Action[ThirtyOneAction]] = removedPlayers.map(p => Action(p, LeaveTable))
      val showCardsHistory: Seq[Action[ThirtyOneAction]] = gameState.players.map(p => Action(p.id, Show, p.hand))
      val returnedCards: Seq[Card] = gameState.players.flatMap(_.hand)
      val gameOver: Boolean = (gameState.players.length - removedPlayers.length) <= 1
      val lastHistory: Seq[Action[ThirtyOneAction]] = gameOver match {
        case false => Nil
        case true => {
          val winner: String = (gameState.players.map(_.id).diff(removedPlayers)).headOption.getOrElse("MISSING")
          updatedPlayers = updatedPlayers.map(p => if (p.id == winner) p.copy(tokens = p.tokens + updatedPot) else p) 
          Seq(Action(winner, Win, Nil, Some(gameState.pot)))
        }
      }
      return gameState.copy(
        history = gameState.history ++ paymentHistory ++ lostPlayerHistory ++ showCardsHistory ++ lastHistory, 
        players = updatedPlayers.filter(p => !removedPlayers.contains(p.id)).map(p => p.copy(hand = Nil, suspectedCards = Nil, suspectedSuitChange = false)),
        knockedPlayerId = None,
        winningPlayerId = None,
        discardPile = Nil,
        deck = gameState.deck.copy(cards = gameState.deck.cards ++ returnedCards ++ gameState.discardPile),
        pot = if (gameOver) 0 else updatedPot,
        round = gameState.round + 1)
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
    val currentScore: Long = evaluation.eval(currentHand)
    val nextPlayer: ThirtyOnePlayerState = gameState.players(gameState.nextPlayerIndex())
    val suspected: Seq[Card] = nextPlayer.suspectedCards
    
    // draw discard logic
    val drawDiscardPermutationsAndScores: Seq[(Seq[Card], Long)] = evaluation.permutationsAndScores(cards = currentHand ++ Seq(gameState.discardPile.head), n = 3)
    val drawDiscardPileHand: Seq[Card] = drawDiscardPermutationsAndScores.length match {
      case 0 => Nil
      case _ => drawDiscardPermutationsAndScores.maxBy(_._2)._1
    }
    val drawDiscardScore: Long =  evaluation.eval(drawDiscardPileHand)
    val drawDiscardPileDiscardedCard: Card = (currentHand ++ Seq(gameState.discardPile.head)).diff(drawDiscardPileHand).head
    if (drawDiscardScore == 32) { // draw from discard if it leads to 31 (instant win)
      return gameState.copy(
        winningPlayerId = Some(currentPlayer.id),
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ 
          Seq(
            Action(currentPlayer.id, DrawFromDiscard, Seq(gameState.discardPile.head)), 
            Action(currentPlayer.id, Discard, Seq(drawDiscardPileDiscardedCard))), 
        players = gameState.updatedHandAndSuspectedCards(
          updatedHand = drawDiscardPileHand, 
          discarded = Seq(drawDiscardPileDiscardedCard),
          suspectedSuitChange = suspectedSuitChange(currentPlayer.id, drawDiscardPileDiscardedCard, gameState),
          publiclyViewedNewCards = Seq(gameState.discardPile.head)),
        discardPile = Seq(drawDiscardPileDiscardedCard) ++ gameState.discardPile.tail)
    }
    val drawDiscardNextPlayerPermutations = evaluation.permutationsAndScores(Seq(drawDiscardPileDiscardedCard) ++ suspected, suspected.length)
    val drawDiscardNextPlayerPotentialScore: Long = drawDiscardNextPlayerPermutations.length match {
      case 0 => 0
      case _ => drawDiscardNextPlayerPermutations.maxBy(_._2)._2
    }
    if (drawDiscardNextPlayerPotentialScore < 32 && drawDiscardScore > currentScore && (drawDiscardScore - currentScore >= 7 || drawDiscardScore >= 30)) {
      return gameState.copy(
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ 
          Seq(
            Action(currentPlayer.id, DrawFromDiscard, Seq(gameState.discardPile.head)), 
            Action(currentPlayer.id, Discard, Seq(drawDiscardPileDiscardedCard))), 
        players = gameState.updatedHandAndSuspectedCards(
          updatedHand = drawDiscardPileHand, 
          discarded = Seq(drawDiscardPileDiscardedCard), 
          suspectedSuitChange = suspectedSuitChange(currentPlayer.id, drawDiscardPileDiscardedCard, gameState),
          publiclyViewedNewCards = Seq(gameState.discardPile.head)),
        discardPile = Seq(drawDiscardPileDiscardedCard) ++ gameState.discardPile.tail)
    }
    
    // early knock logic
    if (!gameState.knockedPlayerId.isDefined && gameState.history.length == 0 && currentScore > 17) {
      // if, during the very first turn, hand is good enough then knock right away
      return gameState.copy (
        knockedPlayerId = Some(currentPlayer.id), 
        currentPlayerIndex = Some(gameState.nextPlayerIndex()),
        history = gameState.history ++ Seq(Action(currentPlayer.id, Knock)))
    }
    // if knocking, should make sure that next player would not get a thirty one automatic win or a higher score from drawing from discard pile
    val otherPlayersSuspectedOfSuitChange: Boolean = gameState.players.filter(_.id != currentPlayer.id).count(_.suspectedSuitChange) > 0
    val acceptableScoreToKnock: Int = otherPlayersSuspectedOfSuitChange match {
      case true => 15 // if any of the other players is suspected of changing suits, then we're more confident of not having lowest score
      case false => 22 
    }
    val nextPlayerDrawDiscardPotentialScore: Long = evaluation.permutationsAndScores(Seq(gameState.discardPile.head) ++ suspected, suspected.length).maxBy(_._2)._2
    if (!gameState.knockedPlayerId.isDefined &&
      // either it's first round or at least 1 other player is suspected of changing suits
      (gameState.history.length < gameState.players.length || otherPlayersSuspectedOfSuitChange) && 
      currentScore >= acceptableScoreToKnock && 
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
    val (drawnCard, updatedDeck): (Seq[Card], Deck) = gameState.deck.deal()
    val drawCardPermutationsAndScores: Seq[(Seq[Card], Long)] = evaluation.permutationsAndScores(cards = currentHand ++ drawnCard, n = 3)
    // determine if next player would have a better or even winning hand if he or she picks up any of this player's discarded cards
    val nextPlayerPotentialScores: Map[Card, Long] = 
      (currentHand ++ drawnCard)
        .map { c =>  
          c -> evaluation
            .permutationsAndScores(Seq(c) ++ suspected, n = suspected.length)
            .maxBy(_._2)
            ._2 
        }
        .toMap

    val discardsToAvoid: Seq[Card] = nextPlayerPotentialScores.filter(_._2 >= 32).toSeq.map(_._1)
    val safePermutationsAndScores: Seq[(Seq[Card], Long)] = drawCardPermutationsAndScores.filter(_._1.diff(discardsToAvoid).nonEmpty)
    // val safePermutationsAndScores: Seq[(Seq[Card], Int)] = discardsToAvoid match {
    //   case Nil => drawCardPermutationsAndScores
    //   case cs => drawCardPermutationsAndScores.filter(_._1.intersect(discardsToAvoid).nonEmpty)
    // }
    val drawHand: Seq[Card] = safePermutationsAndScores.length match {
      // case 0 => Nil // currentHand ++ drawnCard // ???
      case 0 => drawnCard // ???
      case _ => safePermutationsAndScores.maxBy(_._2)._1
    }
    val drawHandScore: Long = evaluation.eval(drawHand)
    val discardedCard: Card = (currentHand ++ drawnCard).diff(drawHand).head
    val drawActionCards: Seq[Card] = gameState.debug match {
      case true => drawnCard // show all cards for debug mode
      case false => Nil // don't display anything by default when player draws from stock
    }
    return gameState.copy(
      currentPlayerIndex = Some(gameState.nextPlayerIndex()),
      history = gameState.history ++ 
        Seq(Action(currentPlayer.id, DrawFromStock, drawActionCards), Action(currentPlayer.id, Discard, Seq(discardedCard))), 
      players = gameState.updatedHandAndSuspectedCards(
        updatedHand = drawHand, 
        suspectedSuitChange = suspectedSuitChange(currentPlayer.id, discardedCard, gameState),
        discarded = Seq(discardedCard)),
      deck = updatedDeck,
      discardPile = Seq(discardedCard) ++ gameState.discardPile)
  }

  def init(playerNames: Seq[String]): ThirtyOneGameState = {
    val players: Seq[ThirtyOnePlayerState] = for (player <- playerNames) yield ThirtyOnePlayerState(s"$player", 4)
    ThirtyOneGameState(players = players)
  }

  def init(playerCount: Int = 1): ThirtyOneGameState = {
    val players: Seq[String] = for (i <- 0 to playerCount) yield s"player${i+1}"
    init(players)
  }


}
