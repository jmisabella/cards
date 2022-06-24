package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.classes.{ Card, SuitedCard, Rank, Suit }
import cards.models.classes.Rank._
import cards.models.classes.Suit._

trait PokerPredicates {
  type CB <: Commons
  val commons: CB 

  def isStraight(cards: Seq[Card]): Boolean = commons.sequenced(cards)

  def isFlush(cards: Seq[Card]): Boolean = 
    commons.countSuit(cards).values.toSeq contains cards.length

  def isStraightFlush(cards: Seq[Card]): Boolean = 
    isStraight(cards) && isFlush(cards)

  def isRoyalFlush(cards: Seq[Card]): Boolean = 
    isStraightFlush(cards) && 
    cards.count(c => Seq(Ten, Jack, Queen, King, Ace).contains(c.rank)) == cards.length

  def isOnePair(cards: Seq[Card]): Boolean = 
    commons.countRank(cards).values.toSeq.count(_ == 2) == 1
  
  def isTwoPair(cards: Seq[Card]): Boolean = 
    commons.countRank(cards).values.toSeq.count(_ == 2) == 2

  def isThreeOfAKind(cards: Seq[Card]): Boolean = 
    commons.countRank(cards).values.toSeq contains 3
  
  def isFourOfAKind(cards: Seq[Card]): Boolean = 
    commons.countRank(cards).values.toSeq contains 4

  def isFullHouse(cards: Seq[Card]): Boolean = 
    isOnePair(cards) && isThreeOfAKind(cards)

  def isHighCard(cards: Seq[Card]): Boolean = 
    !isOnePair(cards) && 
    !isTwoPair(cards) && 
    !isStraight(cards) && 
    !isFlush(cards) && 
    !isThreeOfAKind(cards) && 
    !isFourOfAKind(cards)

  def filterByPredicate(cards: Seq[Card], p: Seq[Card] => Boolean)(f: Seq[SuitedCard] => Seq[Card]): Option[Seq[Card]] = (commons.suited(cards), p(cards)) match {
    case (Nil, _) => None
    case (_, false) => None
    case (cs, true) => Some(f(cs))  
  }

  def cardByPredicate(cards: Seq[Card], p: Seq[Card] => Boolean)(f: Seq[SuitedCard] => Card): Option[Card] = (commons.suited(cards), p(cards)) match {
    case (Nil, _) => None
    case (_, false) => None
    case (cs, true) => Some(f(cs))  
  }

  def highCard(cards: Seq[Card]): Option[Card] = cardByPredicate(cards, isHighCard) (cs => cs.sorted.reverse.head)

  def onePair(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isOnePair) { cs => 
    cs.groupBy(_.rank).filter(_._2.length == 2).head._2
  }
  
  def twoPair(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isTwoPair) { cs =>
    cs.groupBy(_.rank).filter(_._2.length == 2).toSeq.flatMap(_._2)
  } 
  
  def threeOfAKind(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isThreeOfAKind) { cs =>
    cs.groupBy(_.rank).filter(_._2.length == 3).toSeq.flatMap(_._2)
  }

  def fourOfAKind(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isFourOfAKind) { cs =>
    cs.groupBy(_.rank).filter(_._2.length == 4).toSeq.flatMap(_._2)
  }

  def fullHouse(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isFullHouse) { cs =>
    cs.diff(threeOfAKind(cs).get) ++ threeOfAKind(cs).get
  }

  // TODO: test
  def straight(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isStraight) (_.sorted)
  
  // TODO: test
  def flush(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isFlush) (cs => cs)
  
  // TODO: test
  def straightFlush(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isStraightFlush) (cs => cs)
  
  // TODO: test
  def royalFlush(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isRoyalFlush) (cs => cs)
  
  // // TODO: test
  // def highCardRank(cards: Seq[Card]): Option[Rank] = (commons.suited(cards), highCard(cards)) match {
  //   case (Nil, _) => None
  //   case (_, false) => None
  //   case (cs, true) => Some(cs.sorted.reverse.head.rank)
    
  // }

  // // TODO: test
  // def onePairRank(cards: Seq[Card]): Option[Rank] = (commons.suited(cards), onePair(cards)) match {
  //   case (Nil, _) => None
  //   case (_, false) => None
  //   case (cs, true) => Some(commons.countRank(cs).filter(_._2 == 2).head._1) 
  // }

  // def twoPairRanks(cards: Seq[Card]): Option[(Rank, Rank)] = (commons.suited(cards), twoPair(cards)) match {
  //   case (Nil, _) => None
  //   case (_, false) => None
  //   case (cs, true) => Some(commons.countRank(cs).filter(_._2 == 2).map(_._1))
  // }


}