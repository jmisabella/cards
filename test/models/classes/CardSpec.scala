package cards.models.classes


import cards.models.classes.{ SuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.FaceDirection._
import org.scalatest.flatspec.AnyFlatSpec

class CardSpec extends AnyFlatSpec {

  "Card" should "be able to change whether face up or face down" in {
    val card1 = SuitedCard(Seven, Clubs)
    val result = card1.copy(direction = Up)

  }
}