package behaviors

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
    When("Textualization is converting action to words")
    val result = words(input) 
    Then("single word hits should be yielded")
    result should equal ("hits")
  }

  it should "make 2-word enumeration value into 2 lower-case words separated by a space with a letter 's' suffix after the 1st word only" in {
    Given("BlackjackAction DoubleDown")
    val input = DoubleDown
    When("Textualization is converting action to words")
    val result = words(input) 
    Then("""2 words "doubles down" should be yielded""")
    result should equal ("doubles down")

  }
}