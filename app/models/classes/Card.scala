package cards.models.classes

object Rank extends Enumeration {
  type Rank = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
}

object Suit extends Enumeration {
  type Suit = Value
  val Clubs, Diamonds, Hearts, Spades = Value
}

object FaceDirection extends Enumeration {
  type FaceDirection = Value
  val Up, Down = Value
}

object Joker extends Enumeration {
  type Joker = Value
  val LeftBower, RightBower = Value
}

import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.FaceDirection._
import cards.models.classes.Joker._

sealed trait Card {
  val isJoker: Boolean
  val direction: FaceDirection = Down

  def copy(isJoker: Boolean = this.isJoker, direction: FaceDirection = this.direction): Card = this match {
    case UnsuitedCard(joker, _) => UnsuitedCard(joker, direction) 
    case SuitedCard(rank, suit, _) => SuitedCard(rank, suit, direction)
  }
}
object Card {
  implicit def ordering[A <: Card]: Ordering[A] = Ordering.by(_.toString)
}

case class SuitedCard(rank: Rank, suit: Suit, override val direction: FaceDirection = Down) extends Card {
  override val isJoker = false
  override def toString(): String = s"$rank of $suit"
  require(!isJoker) 
}

case class UnsuitedCard(rank: Joker, override val direction: FaceDirection = Down) extends Card {
  override val isJoker = true
  override def toString(): String = rank match {
    case LeftBower => "Left Bower Joker"
    case RightBower => "Right Bower Joker"
  }
  require(isJoker) 
}

