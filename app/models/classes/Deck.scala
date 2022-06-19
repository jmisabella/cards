package cards.models.classes

import cards.models.utilities.RNG
import cards.models.classes.{ Card, SuitedCard, Rank, Suit }
import cards.models.classes.Joker.{ LeftBower, RightBower }
import cards.models.classes.Rank._
import scala.util.Random

case class Deck private (cards: List[Card], seed: RNG) {
  def foreach(block: Card => Unit): Unit = cards.foreach(block)
  def map(f: Card => Card): Deck = Deck(cards.map(f), seed)
  def withFilter(p: Card => Boolean): Deck = Deck(cards.filter(p), seed)
  def filter(p: Card => Boolean): Deck = withFilter(p)
  def size(): Int = cards.length
}

object Deck {
  def allCards(): Deck = 
    Deck( 
      (for { r <- Rank.values; s <- Suit.values } yield SuitedCard(r, s)).toList ++ List(UnsuitedCard(LeftBower), UnsuitedCard(RightBower))
      , RNG.RandomSeed(Random.nextInt(54 + 1)))

  def jokersExcluded(): Deck = allCards().filter(!_.isJoker).copy(seed = RNG.RandomSeed(Random.nextInt(52 + 1)))

  def euchreDeck(): Deck = {
    jokersExcluded()
      .filter(c => !Seq(Two, Three, Four, Five, Six, Seven, Eight).contains(c.asInstanceOf[SuitedCard].rank))
      .copy(seed = RNG.RandomSeed(Random.nextInt(24 + 1)))
  }

  def blackJackShoe(numberOfShoes: Int): Deck = {
    // Deck( (for (i <- 0 until numberOfShoes) yield jokersExcluded()).toList.flatten, RNG.nextInt(52*numberOfShoes+1))
  
    ???
  }

}
