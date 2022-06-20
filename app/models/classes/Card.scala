package cards.models.classes

// object Rank extends Enumeration {
//   type Rank = Value
//   val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
// }

sealed trait Rank extends Ordered[Rank] {
  override def compare(that: Rank): Int = (this, that) match { 
    case (x1, x2) if (x1 == x2) => 0
    case (_, Rank.Two) => 1
    case (x, Rank.Three) if (Seq(Rank.Four, Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Four) if (Seq(Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Five) if (Seq(Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Six) if (Seq(Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Seven) if (Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Eight) if (Seq(Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Nine) if (Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Ten) if (Seq(Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Jack) if (Seq(Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.Queen) if (Seq(Rank.King, Rank.Ace).contains(x)) => 1
    case (x, Rank.King) if (Seq(Rank.Ace).contains(x)) => 1
    case (Rank.Ace, _) => 1
    case (Rank.Two, _) => -1
    case (Rank.Three, x) if (Seq(Rank.Four, Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Four, x) if (Seq(Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Five, x) if (Seq(Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Six, x) if (Seq(Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Seven, x) if (Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Eight, x) if (Seq(Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Nine, x) if (Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Ten, x) if (Seq(Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Jack, x) if (Seq(Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.Queen, x) if (Seq(Rank.King, Rank.Ace).contains(x)) => -1
    case (Rank.King, x) if (Seq(Rank.Ace).contains(x)) => -1
    case (_, Rank.Ace) => -1
  }
}

object Rank {
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank
  val values: Seq[Rank] = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
}

sealed trait Suit
object Suit {
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit
  val values: Seq[Suit] = Seq(Clubs, Diamonds, Hearts, Spades)
}
// object Suit extends Enumeration {
//   type Suit = Value
//   val Clubs, Diamonds, Hearts, Spades = Value
// }

// sealed trait FaceDirection
// object FaceDirection {
//   case object Up extends FaceDirection
//   case object Down extends FaceDirection
//   val values: Seq[FaceDirection] = Seq(Up, Down)
// }
// object FaceDirection extends Enumeration {
//   type FaceDirection = Value
//   val Up, Down = Value
// }

object Joker extends Enumeration {
  type Joker = Value
  val LeftBower, RightBower = Value
}

import cards.models.classes.Rank._
import cards.models.classes.Suit._
// import cards.models.classes.FaceDirection._
import cards.models.classes.Joker._

sealed trait Card {
  val isJoker: Boolean

  def copy(isJoker: Boolean = this.isJoker): Card = this match {
    case UnsuitedCard(joker) => UnsuitedCard(joker) 
    case SuitedCard(rank, suit) => SuitedCard(rank, suit)
  }

  // override def compare(that: Card): Int = (this, that) match {
  //   case (x1, x2) if (x1 == x2) => 0
  //   case (_, Rank.Two) => 1
  //   case (x, Rank.Three) if (Seq(Rank.Four, Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Four) if (Seq(Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Five) if (Seq(Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Six) if (Seq(Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Seven) if (Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Eight) if (Seq(Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Nine) if (Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Ten) if (Seq(Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Jack) if (Seq(Rank.Queen, Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.Queen) if (Seq(Rank.King, Rank.Ace).contains(x)) => 1
  //   case (x, Rank.King) if (Seq(Rank.Ace).contains(x)) => 1
  //   case (Rank.Ace, _) => 1
  //   case (Rank.Two, _) => -1
  //   case (Rank.Three, x) if (Seq(Rank.Four, Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Four, x) if (Seq(Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Five, x) if (Seq(Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Six, x) if (Seq(Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Seven, x) if (Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Eight, x) if (Seq(Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Nine, x) if (Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Ten, x) if (Seq(Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Jack, x) if (Seq(Rank.Queen, Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.Queen, x) if (Seq(Rank.King, Rank.Ace).contains(x)) => -1
  //   case (Rank.King, x) if (Seq(Rank.Ace).contains(x)) => -1
  //   case (_, Rank.Ace) => -1
  // }

}
object Card {
  implicit def ordering[A <: Card]: Ordering[A] = Ordering.by(_.toString)
}

case class SuitedCard(rank: Rank, suit: Suit) extends Card {
  override val isJoker = false
  override def toString(): String = s"$rank of $suit"
  require(!isJoker) 
}

case class UnsuitedCard(rank: Joker) extends Card {
  override val isJoker = true
  override def toString(): String = rank match {
    case LeftBower => "Left Bower Joker"
    case RightBower => "Right Bower Joker"
  }
  require(isJoker) 
}

