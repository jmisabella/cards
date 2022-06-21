package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.behaviors.predicates.PokerPredicates
import cards.models.classes.{ Card, SuitedCard, UnsuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class PokerPredicatesSpec extends AnyFlatSpec {

  private[predicates] case object _commons extends Commons
  case object module extends PokerPredicates {
    override type CB = Commons
    override val commons = _commons
  }

  "PokerPredicates" should "observe a royal flush" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Clubs), 
      SuitedCard(Jack, Clubs), 
      SuitedCard(Queen, Clubs), 
      SuitedCard(King, Clubs),
      SuitedCard(Ace, Clubs))

    val result = module.royalFlush(cards)
    assert(result)
  }

  it should "observe when it's not a royal flush" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Jack, Clubs), 
      SuitedCard(Queen, Diamonds), 
      SuitedCard(King, Clubs),
      SuitedCard(Ace, Clubs))

    val result = module.royalFlush(cards)
    assert(!result)
  }
  
  it should "observe when it's a flush" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Hearts), 
      SuitedCard(Queen, Hearts), 
      SuitedCard(Seven, Hearts),
      SuitedCard(Ace, Hearts))

    val result = module.flush(cards)
    assert(result)
  }
  
  it should "observe when it's a straight" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Jack, Clubs), 
      SuitedCard(Queen, Diamonds), 
      SuitedCard(King, Clubs),
      SuitedCard(Ace, Clubs))

    val result = module.straight(cards)
    assert(result)
  }

  it should "observe when it's a high card" in {
    val cards: Seq[Card] = Seq(
      SuitedCard(Ten, Hearts), 
      SuitedCard(Two, Clubs), 
      SuitedCard(Queen, Diamonds), 
      SuitedCard(Ace, Hearts),
      SuitedCard(Five, Clubs))

    val result = module.highCard(cards)
    assert(result)
  }

}