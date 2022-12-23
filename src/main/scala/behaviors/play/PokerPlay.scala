package cards.behaviors.play

import cards.classes.hand.Hand
import cards.classes.Card

trait PokerPlay {

  def cards(hands: Seq[Hand], playerId: String): Seq[Card] = hands.filter(_.owners.contains(playerId)).flatMap(_.hand)

}
