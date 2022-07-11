package cards.models.behaviors.serialization

import cards.models.behaviors.serialization.ThirtyOneActionSerialization
import cards.models.classes.{ Card, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.actions.ThirtyOneAction
import org.scalatest.flatspec.AnyFlatSpec

class ThirtyOneActionSerializationSpec extends AnyFlatSpec {
  case object module extends ThirtyOneActionSerialization

  "ThirtyOneActionSerialization" should "???" in {
    // TODO: implement
  }
}