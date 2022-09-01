package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.options.BlackjackOptions

trait BlackjackPredicates {
  type CB <: Commons
  val commons: CB 

  // Only players can split on first play (only 2 cards) when both cards have same value
  // Tens, Jacks, Queens, and Kings are all considered the same value, meaning that player can split on Ten and Queen, etc... 
  // Dealers cannot split, but this function doesn't check for this
  // TODO: need to check blackjack options regarding resplitLimit and resplitOnSplitAces
  def canSplit(cards: Seq[Card], options: BlackjackOptions = BlackjackOptions()): Boolean = options.splitOnRankMatchOnly match {
    case true => cards.length == 2 && commons.countRank(cards).values.toSeq.contains(2)
    case false => {
      val tens: Seq[Rank] = Seq(Ten, Jack, Queen, King)
      cards.length == 2 && 
        (tens.contains(cards.head.rank) && tens.contains(cards.tail.head.rank) || commons.countRank(cards).values.toSeq.contains(2))
    }
  }
  
  // Players eligible for insurance only if dealer's first hand (only 2 cards) shows an ace as the face-up card  
  // Dealer's first card in hand is considered the face-up card 
  def eligibleForInstance(dealerCards: Seq[Card]): Boolean =
    // only eligible if first turn (dealer only has 2 cards) and the face up card (first card) is an Ace 
    dealerCards.length == 2 && dealerCards.head.rank == Ace

}