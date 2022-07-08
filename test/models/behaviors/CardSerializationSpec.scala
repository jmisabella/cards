package cards.models.behaviors

import cards.models.behaviors.CardSerialization
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class CardSerializationSpec extends AnyFlatSpec {
  case object module extends CardSerialization

  "CardSerialization" should "create valid json from an empty" in {
    val cards: Seq[Card] = Nil
    val json = module.json(cards)
    println("JSON: " + json)
    val result: Either[String, Seq[Card]] = module.parse(json)
    assert(result.isRight)
    result match {
      case Right(cs) => println("RESULT: " + cs)
      case Left(e) => println("ERROR IN RESULT: " + e)
    }
  }

  it should "create valid json from a hand with a single two of spades" in {
    val cards: Seq[Card] = Seq(Card(Two, Spades))
    val json = module.json(cards)
    println("JSON: " + json)
    val result: Either[String, Seq[Card]] = module.parse(json)
    assert(result.isRight)
    result match {
      case Right(cs) => println("RESULT: " + cs)
      case Left(e) => println("ERROR IN RESULT: " + e)
    }
  }


  it should "create valid json from a 2 card hand with two of spades and three of hearts" in {
    val cards: Seq[Card] = Seq(Card(Two, Spades), Card(Three, Hearts))
    val json = module.json(cards)
    println("JSON: " + json)
    val result: Either[String, Seq[Card]] = module.parse(json)
    assert(result.isRight)
    result match {
      case Right(cs) => println("RESULT: " + cs)
      case Left(e) => println("ERROR IN RESULT: " + e)
    }
  }

  it should "create valid json from a three card hand with 2 suited cards and a Joker" in {
    val cards: Seq[Card] = Seq(Card(LeftBower, Joker), Card(Two, Spades), Card(Three, Hearts))
    val json = module.json(cards)
    println("JSON: " + json)
    val result: Either[String, Seq[Card]] = module.parse(json)
    assert(result.isRight)
    result match {
      case Right(cs) => println("RESULT: " + cs)
      case Left(e) => println("ERROR IN RESULT: " + e)
    }
  }

}