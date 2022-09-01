package cards.models.behaviors.serialization

import cards.models.behaviors.serialization.BlackjackActionSerialization
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.actions.Action
import cards.models.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers._

class BlackjackActionSerializationSpec extends AnyFlatSpec with GivenWhenThen {
  case object module extends BlackjackActionSerialization
  private val right = Symbol("right")
  private val left = Symbol("left")

  "BlackjackActionSerialization" should "create valid json from Bet action even if the amount is not specified" in {
    Given("a Bet action but without any action amount specified (an empty bet)") 
    val action: Seq[Action[BlackjackAction]] = Seq(Action[BlackjackAction]("JMI", Bet))

    When("serializing the action object to json")
    val json: String = module.json(action)

    Then("the parsed JSON result should be valid JSON and successfully deserialize into the same Blackjack action")
    val result: Either[String, Seq[Action[BlackjackAction]]] = module.parse(json)
    result should be (right) 
    result should equal (Right(action))
  }

  it should "create valid json from Bet action when an amount is specified" in {
    Given("a Bet action with an amount of 20 specified") 
    val action: Seq[Action[BlackjackAction]] = Seq(Action[BlackjackAction]("JMI", Bet, Nil, 20))

    When("serializing the action object to json")
    val json: String = module.json(action)

    Then("the parsed JSON result should be valid JSON and successfully deserialize into the same Blackjack action")
    val result: Either[String, Seq[Action[BlackjackAction]]] = module.parse(json)
    result should be (right) 
    result should equal (Right(action))
  }
}
