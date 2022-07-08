package cards.models.classes

import cards.models.classes.Card

// TODO: behaviors, json serialization, testing
case class PlayerAction[A <: Enumeration](previousCards: Seq[Card], action: A, actionCards: Seq[Card], updatedCards: Seq[Card]) {
  // override def toString(): String = s"""previous [${previousCards.mkString(", ")}], action [$action], action cards [${actionCards.mkString(", ")}], updated [${updatedCards.mkString(", ")}]"""
}
