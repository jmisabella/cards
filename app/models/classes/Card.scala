package cards.models.classes

import play.api.libs.json.{ Json, Format }

object Rank extends Enumeration {
  type Rank = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace, LeftBower, RightBower = Value
  
  implicit def ordering [A <: Rank]: Ordering[A] = Ordering.by(_.id)
  implicit val format: Format[Rank] = Json.formatEnum(this)
  
  val suited: Seq[Rank.Value] = values.toSeq.filter(r => !Seq(LeftBower, RightBower).contains(r))
  val unsuited: Seq[Rank.Value] = values.toSeq.filter(r => Seq(LeftBower, RightBower).contains(r))
}

object Suit extends Enumeration {
  type Suit = Value
  val Clubs, Diamonds, Hearts, Spades, Joker = Value
  
  implicit val format: Format[Suit] = Json.formatEnum(this)
  
  val suited: Seq[Suit.Value] = values.toSeq.filter(r => r != Joker)
}

import cards.models.classes.Rank._
import cards.models.classes.Suit._

case class Card(rank: Rank, suit: Suit) {
  val isJoker: Boolean = rank == LeftBower || rank == RightBower
  implicit def ordering[A <: Card]: Ordering[A] = Ordering.by(_.rank.id)
  override def toString(): String = {
    (Json.obj(
      "rank" -> rank,
      "suit" -> suit
    )).toString()
  }

  // override def toString(): String = (Json.obj (
  //   "suited-card" -> Json.obj(
  //       "rank" -> rank,
  //       "suit" -> suit,
  //     ))).toString()

}
object Card {
  implicit val format: Format[Card] = Json.format[Card]
  implicit def ordering[A <: Card]: Ordering[A] = Ordering.by(_.rank.id)
}

case class Cards(cards: Seq[Card]) {
  override def toString(): String = (cards.map(_.toString())).mkString("""{"cards":[""", "," ,"]}") 
}

object Cards {
  implicit val format: Format[Cards] = Json.format[Cards]
}

// sealed trait Card {
//   val rank: Rank
//   val isJoker: Boolean = rank == LeftBower || rank == RightBower 
//   override def toString(): String = (this match {
//     case SuitedCard(rank, suit) => Json.obj("rank" -> rank, "suit" -> suit)
//     case UnsuitedCard(rank) => Json.obj("rank" -> rank)
//   }).toString()
// }

// object Card {
//   implicit def ordering[A <: Card]: Ordering[A] = Ordering.by(_.rank.id)
//   implicit val format: Format[Card] = Json.format[Card]
    
// }


// case class SuitedCard(override val rank: Rank, suit: Suit) extends Card {
//   require(!isJoker, s"SuitedCard is not supposed to have isJoker but it does; rank [$rank], suit [$suit]")
//   implicit def ordering[A <: SuitedCard]: Ordering[A] = Ordering.by(_.rank.id)
//   override def toString(): String = {
//     (Json.obj(
//       "rank" -> rank,
//       "suit" -> suit
//     )).toString()
//   }

//   // override def toString(): String = (Json.obj (
//   //   "suited-card" -> Json.obj(
//   //       "rank" -> rank,
//   //       "suit" -> suit,
//   //     ))).toString()

// }
// object SuitedCard {
//   implicit val format: Format[SuitedCard] = Json.format[SuitedCard]
// }

// case class UnsuitedCard(override val rank: Rank) extends Card {
//   override def toString(): String = {
//     (Json.obj(
//       "rank" -> rank,
//     )).toString()
//   }
//   // override def toString(): String = (Json.obj (
//   //   "unsuited-card" -> Json.obj(
//   //       "rank" -> rank,
//   //     ))).toString()
  
//   implicit def ordering[A <: UnsuitedCard]: Ordering[A] = Ordering.by(_.rank.id)
//   require(isJoker, s"UnsuitedCard is supposed to have isJoker but it does not; rank [$rank]")
// }

// object UnsuitedCard {
//   implicit val format: Format[UnsuitedCard] = Json.format[UnsuitedCard]
// }
