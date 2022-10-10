package cards.behaviors.serialization

import cards.behaviors.serialization.CardSerialization
import cards.classes.{ Card, Rank, Suit }
import cards.classes.Rank._
import cards.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CardSerializationSpec extends AnyFlatSpec {
  case object module extends CardSerialization

  private val right = Symbol("right")
  private val left = Symbol("left")
  
  "CardSerialization" should "create valid json from an empty" in {
    val cards: Seq[Card] = Nil
    val json = module.json(cards)
    val result: Either[String, Seq[Card]] = module.parse(json)
    result should be (right)
    result should equal (Right(cards))
  }

  it should "create valid json from a hand with a single two of spades" in {
    val cards: Seq[Card] = Seq(Card(Two, Spades))
    val json = module.json(cards)
    val result: Either[String, Seq[Card]] = module.parse(json)
    result should be (right) 
    result should equal (Right(cards))
  }


  it should "create valid json from a 2 card hand with two of spades and three of hearts" in {
    val cards: Seq[Card] = Seq(Card(Two, Spades), Card(Three, Hearts))
    val json = module.json(cards)
    val result: Either[String, Seq[Card]] = module.parse(json)
    result should be (right) 
    result should equal (Right(cards))
  }

  it should "create valid json from a three card hand with 2 suited cards and a Joker" in {
    val cards: Seq[Card] = Seq(Card(LeftBower, Joker), Card(Two, Spades), Card(Three, Hearts))
    val json = module.json(cards)
    val result: Either[String, Seq[Card]] = module.parse(json)
    result should be (right) 
    result should equal (Right(cards))
  }

}