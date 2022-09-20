package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.BlackjackPredicates
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.options.BlackjackOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackJackPredicatesSpec extends AnyFlatSpec with GivenWhenThen {
  private[predicates] case object _commons extends Commons
  case object module extends BlackjackPredicates {
    override type CB = Commons
    override val commons = _commons
  }


  "BlackjackPredicates" should "not allow players insurance if dealer's hand is an empty list" in {
    val dealerCards: Seq[Card] = Nil
    val result: Boolean = module.eligibleForInsurance(dealerCards)
    result should be (false)
  }
  
  it should "not allow players insurance if dealer's hand has more than 1 card" in {
    val dealerCards: Seq[Card] = Seq(Card(Ace, Spades), Card(Seven, Hearts), Card(Three, Spades)) 
    val result: Boolean = module.eligibleForInsurance(dealerCards)
    result should be (false)
  }

  it should "allow players insurance if dealer's hand has exactly 2 cards and its face-up card (first card) is an Ace" in {
    val dealerCards: Seq[Card] = Seq(Card(Ace, Hearts), Card(Two, Clubs))
    val result: Boolean = module.eligibleForInsurance(dealerCards)
    result should be (true)
  }


}