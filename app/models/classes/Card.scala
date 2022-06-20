package cards.models.classes

object Rank extends Enumeration {
  type Rank = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
  implicit def ordering [A <: Rank]: Ordering[A] = Ordering.by(_.id)
}

// sealed trait Suit
// object Suit {
//   case object Clubs extends Suit
//   case object Diamonds extends Suit
//   case object Hearts extends Suit
//   case object Spades extends Suit
//   val values: Seq[Suit] = Seq(Clubs, Diamonds, Hearts, Spades)
// }

object Suit extends Enumeration {
  type Suit = Value
  val Clubs, Diamonds, Hearts, Spades = Value
}

object Joker extends Enumeration {
  type Joker = Value
  val LeftBower, RightBower = Value
  implicit def ordering [A <: Joker]: Ordering[A] = Ordering.by(_.id)
}

import cards.models.classes.Rank._
import cards.models.classes.Suit._
// import cards.models.classes.FaceDirection._
import cards.models.classes.Joker._

sealed trait Card { //extends Ordered[Card] {
  val isJoker: Boolean

  def copy(isJoker: Boolean = this.isJoker): Card = this match {
    case UnsuitedCard(joker) => UnsuitedCard(joker) 
    case SuitedCard(rank, suit) => SuitedCard(rank, suit)
  }

}

object Card {
  // implicit def ordering[A <: Card]: Ordering[A] = Ordering.by { c => 
  //   if (c.isInstanceOf[UnsuitedCard]) 
  //     c.asInstanceOf[UnsuitedCard].rank.id
  //   else 
  //     c.asInstanceOf[SuitedCard].rank.id
  // }

  // implicit def ordering[A <: Card]: Ordering[A] = new Ordering[A] {
  //   override def compare(x: A, y: A): Int = {
  //     x.toString.compareTo(y.toString)
  //   }
  // }
  
  implicit def ordering[A <: Card]: Ordering[A] = Ordering.by(_.toString)

  // implicit def ordering[A <: Card]: Ordering[A] = new Ordering[A] {
  //   override def compare(x: A, y: A): Int = (x, y) match {
  //     case (_, _) if (x == y) => 0
  //     case (UnsuitedCard(_), SuitedCard(_, _)) => 1
  //     case (SuitedCard(_, _), UnsuitedCard(_)) => -1 
  //     case (UnsuitedCard(joker1), UnsuitedCard(joker2)) => joker1.compareTo(joker2)
  //     case (SuitedCard(rank1, _), SuitedCard(rank2, _)) => rank1.compareTo(rank2)
  //   }
  // }


  // implicit def ordering[A <: Card]: Ordering[A] = new Ordering[A] {
  //   override def compare(x: A, y: A): Int = (x, y) match {
  //     // case (_, _) if (x == y) => 0
  //     case (UnsuitedCard(_), SuitedCard(_, _)) => 1 
  //     case (SuitedCard(_, _), UnsuitedCard(_)) => -1 
  //     case (UnsuitedCard(joker1), UnsuitedCard(joker2)) => joker1.compare(joker2)
  //     case (SuitedCard(rank1, _), SuitedCard(rank2, _)) => rank1.compare(rank2)
  //   }
  // }


  // implicit def ordering[A <: Card]: Ordering[A] = new Ordering[A] {
  //   override def compare(x: A, y: A): Int = (x, y) match {
  //     case (_, _) if (x == y) => 0
  //     case (UnsuitedCard(joker1), UnsuitedCard(joker2)) => joker1.compare(joker2)
  //     case (_, UnsuitedCard(_)) => -1
  //     case (UnsuitedCard(_), _) => 1
  //     case (SuitedCard(rank1, _), SuitedCard(rank2, _)) => rank1.compare(rank2)
  //   }
  // }
}

case class SuitedCard(rank: Rank, suit: Suit) extends Card {
  override val isJoker = false
  override def toString(): String = s"$rank of $suit"
  require(!isJoker) 
  implicit def ordering[A <: SuitedCard]: Ordering[A] = Ordering.by(_.rank.id)
  
  // override def compare(that: Card): Int = this.asInstanceOf[Card].compare(that)
}

case class UnsuitedCard(rank: Joker) extends Card {
  override val isJoker = true
  override def toString(): String = rank match {
    case LeftBower => "Left Bower Joker"
    case RightBower => "Right Bower Joker"
  }
  implicit def ordering[A <: UnsuitedCard]: Ordering[A] = Ordering.by(_.rank.id)
  // override def compare(that: Card): Int = this.asInstanceOf[Card].compare(that)
  require(isJoker) 
}

