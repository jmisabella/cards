package cards.models.behaviors.serialization

import cards.models.behaviors.serialization.ThirtyOneActionSerialization
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.actions.Action
import cards.models.classes.actions.ThirtyOneAction._
import org.scalatest.flatspec.AnyFlatSpec

class ThirtyOneActionSerializationSpec extends AnyFlatSpec {
  case object module extends ThirtyOneActionSerialization

  "ThirtyOneActionSerialization" should "create valid json from Draw action even if all card collections are empty" in {
    // val action: Seq[Action[ThirtyOneAction]] = Seq(Action[ThirtyOneAction](Draw, Seq(Card(Two, Diamonds)), 0, Nil, Seq(Card(Two, Diamonds))))
    val action: Seq[Action[ThirtyOneAction]] = Seq(Action[ThirtyOneAction](Draw))
    val json: String = module.json(action)
    println("JSON: " + json)
    val result: Either[String, Seq[Action[ThirtyOneAction]]] = module.parse(json)
    assert(result.isRight)
    result match {
      case Right(cs) => println("RESULT: " + cs)
      case Left(e) => println("ERROR IN RESULT: " + e)
    }
  }

  it should "create valid json from Draw action on an empty hand to a single card hand with a two of spades" in {
    val action: Seq[Action[ThirtyOneAction]] = Seq(Action[ThirtyOneAction](Draw, Seq(Card(Two, Spades)), 0, Nil, Seq(Card(Two, Spades))))
    // val action: Seq[Action[ThirtyOneAction]] = Seq(Action[ThirtyOneAction](Draw))
    val json: String = module.json(action)
    println("JSON: " + json)
    val result: Either[String, Seq[Action[ThirtyOneAction]]] = module.parse(json)
    assert(result.isRight)
    result match {
      case Right(cs) => println("RESULT: " + cs)
      case Left(e) => println("ERROR IN RESULT: " + e)
    }

  }


}