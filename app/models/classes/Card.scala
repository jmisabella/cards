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
  override def toString(): String = {
    (Json.obj(
      "rank" -> rank,
      "suit" -> suit
    )).toString()
  }
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
