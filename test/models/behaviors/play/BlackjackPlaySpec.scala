package cards.models.behaviors.play

import cards.models.behaviors.play.BlackjackPlay
import cards.models.classes.{ Card, Rank, Suit, Deck, DeckType }
import cards.models.classes.DeckType._
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.hand.Hand
import cards.models.classes.state.{ BlackjackGameState, BlackjackPlayerState }
import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.actions.{ Action, BlackjackAction }
import cards.models.classes.actions.BlackjackAction._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BlackjackPlaySpec extends AnyFlatSpec with GivenWhenThen {
  private case object _play extends BlackjackPlay
  import _play._

  "BlackjackPlay" should "know when it's not yet time to play because bets have not yet been taken" in {

    pending
  }

  it should 
  "know when it's time to play because bets have been taken and the player has 2 or more cards and hand is not yet flagged as either 'won' or 'lost'" in {

    pending
  }

  it should "throw an illegal argument exception when attempting to play but there are no players" in {

    pending
  }

  // TODO: 

}
