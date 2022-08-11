package cards.models.behaviors.serialization

import cards.models.behaviors.serialization.ThirtyOneActionSerialization
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.actions.Action
import cards.models.classes.actions.ThirtyOneAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ThirtyOneActionSerializationSpec extends AnyFlatSpec {
  case object module extends ThirtyOneActionSerialization

  private val right = Symbol("right")
  private val left = Symbol("left")

  "ThirtyOneActionSerialization" should "create valid json from Draw action even if all card collections are empty" in {
    val action: Seq[Action[ThirtyOneAction]] = Seq(Action[ThirtyOneAction]("JMI", DrawFromStock))
    val json: String = module.json(action)
    val result: Either[String, Seq[Action[ThirtyOneAction]]] = module.parse(json)
    result should be (right) 
  }

  it should "create valid json from Draw action from an empty hand to a single card hand with a two of spades" in {
    val action: Seq[Action[ThirtyOneAction]] = 
      Seq(
        Action[ThirtyOneAction](
          playerId="JMI",
          action=DrawFromStock, 
          actionCards=Seq(Card(Two, Spades)), 
          actionTokens=0 ))

    val json: String = module.json(action)
    val result: Either[String, Seq[Action[ThirtyOneAction]]] = module.parse(json)
    result should be (right) 
  }

}