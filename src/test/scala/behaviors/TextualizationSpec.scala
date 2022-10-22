package behaviors

import cards.classes.Card
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.actions.Action
import cards.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class TextualizationSpec extends AnyFlatSpec with GivenWhenThen {
  case object module extends Textualization
  import module._
  
  "Textualization" should "make single word enumeration value lowercase with a letter 's' suffix" in {
    Given("BlackjackAction Hit")
    val input = Hit
    When("converting action to words")
    val result = words(input, true)
    Then("single word hits should be yielded")
    result should equal ("hits")
  }

  it should "make 2-word enumeration value into 2 lower-case words jeparated by a space with a letter 's' suffix after the 1st word only" in {
    Given("BlackjackAction DoubleDown")
    val input = DoubleDown
    When("converting action to words")
    val result = words(input, true)
    Then("""2 words "doubles down" should be yielded""")
    result should equal ("doubles down")
  }

  it should "make a single sentence for a BlackjackAction involving action cards but no before or after hand specified" in {
    Given("Action in which player Hits in Blackjack, and action card is shown, but before/after hands are not specified")
    val action = Action("Jeffrey", Hit, Seq(Card(Nine, Clubs)))
    When("converting action to words")
    val result = words(action)
    Then(s"""One complete sentence should be rendered""")
    result should equal ("Jeffrey hits: [Nine of Clubs]")
  }

  it should "make three sentences for a BlackjackAction involving action cards with before and after hands both specified" in {
    Given("Action in which player Hits in Blackjack, and action card is shown with before and after hands both specified")
    val action = 
      Action(
        "Jeffrey", 
        Hit, 
        Seq(Card(Ten, Diamonds)), 
        0, 
        Seq(Card(Two, Hearts), Card(Eight, Clubs)), 
        Seq(Seq(Card(Two, Hearts), Card(Eight, Clubs), Card(Ten, Diamonds))))
    When("converting action to words")
    val result = words(action)
    Then(s"""One complete sentence should be rendered""")
    result should equal ("Jeffrey starts with [Two of Hearts, Eight of Clubs]\r\nJeffrey hits: [Ten of Diamonds]\r\nJeffrey ends with [Two of Hearts, Eight of Clubs, Ten of Diamonds]")
  }

}