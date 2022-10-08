package cards.classes.options

import cards.classes.options.BlackjackOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BlackjackOptionsSpec extends AnyFlatSpec {
  "BlackjackOptions" should "allow between 1 and 8 decks to be used but disallow any number of decks outside of the allowed range 1-8" in {
    BlackjackOptions(1)
    BlackjackOptions(2)
    BlackjackOptions(3)
    BlackjackOptions(4)
    BlackjackOptions(5)
    BlackjackOptions(6)
    BlackjackOptions(7)
    BlackjackOptions(8)
    an [IllegalArgumentException] should be thrownBy BlackjackOptions(0)
    an [IllegalArgumentException] should be thrownBy BlackjackOptions(9)
  }
}