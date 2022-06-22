package cards.models.classes

object Rank extends Enumeration {
  type Rank = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace, LeftBower, RightBower = Value
  implicit def ordering [A <: Rank]: Ordering[A] = Ordering.by(_.id)
  val suited: Seq[Rank.Value] = values.toSeq.filter(r => !Seq(LeftBower, RightBower).contains(r))
  val unsuited: Seq[Rank.Value] = values.toSeq.filter(r => Seq(LeftBower, RightBower).contains(r))
}

object Suit extends Enumeration {
  type Suit = Value
  val Clubs, Diamonds, Hearts, Spades = Value
}

import cards.models.classes.Rank._
import cards.models.classes.Suit._

sealed trait Card {
  val rank: Rank
  val isJoker: Boolean

  def copy(isJoker: Boolean = this.isJoker): Card = this match {
    case UnsuitedCard(joker) => UnsuitedCard(joker) 
    case SuitedCard(rank, suit) => SuitedCard(rank, suit)
  }
}

object Card {
  implicit def ordering[A <: Card]: Ordering[A] = Ordering.by(_.rank.id)
}

case class SuitedCard(override val rank: Rank, suit: Suit) extends Card {
  override val isJoker = false
  override def toString(): String = s"$rank of $suit"
  require(!isJoker, s"SuitedCard is not supposed to have isJoker but it does; rank [$rank], suit [$suit]")
  require(!Seq(LeftBower, RightBower).contains(rank), s"SuitedCard's rank [$rank] is not supposed to be LeftBower or RightBower jokers")
  implicit def ordering[A <: SuitedCard]: Ordering[A] = Ordering.by(_.rank.id)
}

case class UnsuitedCard(override val rank: Rank) extends Card {
  override val isJoker = true
  override def toString(): String = rank match {
    case LeftBower => "Left Bower Joker"
    case RightBower => "Right Bower Joker"
  }
  implicit def ordering[A <: UnsuitedCard]: Ordering[A] = Ordering.by(_.rank.id)
  require(isJoker, s"UnsuitedCard is supposed to have isJoker but it does not; rank [$rank]")
  require(!Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace).contains(rank), s"UnsuitedCard's rank [$rank] is supposed to only be either LeftBower or RightBower joker, but it's not")
}

