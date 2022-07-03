package cards.models.behaviors

import cards.models.behaviors.CardSerialization
import cards.models.classes.{ Card, SuitedCard, UnsuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import org.scalatest.flatspec.AnyFlatSpec

class CardSerializationSpec extends AnyFlatSpec {
  case object module extends CardSerialization

  "CardSerialization" should "serialize an empty list as valid json" in {
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
}