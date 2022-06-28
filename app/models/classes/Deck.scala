package cards.models.classes

import cards.models.utilities.RNG
import cards.models.classes.{ Card, SuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import scala.util.Random

sealed trait DeckType
object DeckType {
  case object AllCards extends DeckType
  case object JokersExcluded extends DeckType
  case object Euchre extends DeckType
}
import DeckType._

case class Deck private (cards: List[Card], seed: RNG) {
  def foreach(block: Card => Unit): Unit = cards.foreach(block)
  def map(f: Card => Card): Deck = Deck(cards.map(f), seed)
  def withFilter(p: Card => Boolean): Deck = Deck(cards.filter(p), seed)
  def filter(p: Card => Boolean): Deck = withFilter(p)
  def contains[A <: Card](c: A): Boolean = cards.contains(c.asInstanceOf[Card])
  def contains[A <: Card](cs: Seq[A]): Boolean = cs.foldLeft(false)((acc, c) => contains(c)) 
  def count[A <: Card](p: Card => Boolean): Int = cards count p 
  val length: Int = cards.length

  def deal(n: Int = 1): (Seq[Card], Deck) = {
    if (cards.length < n) {
      throw new IllegalArgumentException(s"Cannot deal [$n] cards because there are only [${cards.length}] cards left in the deck")
    }
    var (dealt, remaining, next): (Seq[Card], List[Card], RNG) = (Nil, cards, seed)
    for (i <- 0 until n) {
      val (index, nextSeed) = next.boundedPositiveInt(remaining.length)
      dealt = dealt ++ Seq(remaining(index))
      remaining = remaining.filter(c => !dealt.contains(c))
      next = nextSeed
    }
    (dealt, Deck(remaining, next))
  }
}

object Deck {
  def apply(deckType: DeckType): Deck = deckType match {
    case AllCards =>  
      Deck( 
        (for { r <- Rank.suited; s <- Suit.values.toSeq } yield SuitedCard(r, s)).toList ++ List(UnsuitedCard(LeftBower), UnsuitedCard(RightBower))
        , RNG.RandomSeed(Random.nextInt(54 + 1)))

    case JokersExcluded =>
      Deck(AllCards).filter(!_.isJoker).copy(seed = RNG.RandomSeed(Random.nextInt(52 + 1)))

    case Euchre => 
      Deck(JokersExcluded)
        .filter(c => !Seq(Two, Three, Four, Five, Six, Seven, Eight).contains(c.asInstanceOf[SuitedCard].rank))
        .copy(seed = RNG.RandomSeed(Random.nextInt(24 + 1)))
  }

  def apply[A <: Card](excluded: Seq[A] = Nil, numberOfDecks: Int = 1): Deck = {
    implicit val view = (d: Deck) => d.cards.toList.iterator
    val available: Deck = Deck(AllCards).filter(c => !excluded.contains(c))
    Deck( 
      (for (i <- 0 until numberOfDecks) yield available).toList.flatten
      , RNG.RandomSeed(Random.nextInt((54 - excluded.length) * numberOfDecks + 1)))
  }
}