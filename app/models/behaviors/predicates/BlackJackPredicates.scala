package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._

trait BlackJackPredicates {
  type CB <: Commons
  val commons: CB 

  // only players can split on first play (only 2 cards) when both cards match rank
  // dealers cannot split, but this function doesn't check for this
  def canSplit(playerCards: Seq[Card]): Boolean = 
    playerCards.length == 2 && commons.countRank(playerCards).values.toSeq.contains(2)

  // players eligible for insurance only if dealer's first hand (only 2 cards) shows an ace as the face-up card  
  def eligibleForInstance(dealerFaceUpCards: Seq[Card]): Boolean =
    // only eligible if first turn (dealer only has 1 face up card) and the face up card is an Ace 
    dealerFaceUpCards.length == 1 && dealerFaceUpCards.head.rank == Ace

}