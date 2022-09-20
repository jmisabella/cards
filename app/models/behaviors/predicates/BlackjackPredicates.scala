package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.options.BlackjackOptions

trait BlackjackPredicates {
  type CB <: Commons
  val commons: CB 

  // TODO: move this function to BlackjackBetting  
  // Players eligible for insurance only if dealer's first hand (only 2 cards) shows an ace as the face-up card  
  // Dealer's first card in hand is considered the face-up card 
  def eligibleForInsurance(dealerCards: Seq[Card]): Boolean =
    // only eligible if first turn (dealer only has 2 cards) and the face up card (first card) is an Ace 
    dealerCards.length == 2 && dealerCards.head.rank == Ace

}