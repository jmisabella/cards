package cards.models.behaviors.predicates

import cards.models.behaviors.Commons
import cards.models.classes.{ Card, SuitedCard, Rank, Suit, PokerHandType }
import cards.models.classes.Rank._
import cards.models.classes.Suit._
import cards.models.classes.PokerHandType._

trait PokerPredicates {
  type CB <: Commons
  val commons: CB 

  def isStraight(cards: Seq[Card]): Boolean = commons.sequenced(cards)

  def isFlush(cards: Seq[Card]): Boolean = 
    cards.length > 1 && (commons.countSuit(cards).values.toSeq contains cards.length)

  def isStraightFlush(cards: Seq[Card]): Boolean = 
    isStraight(cards) && isFlush(cards)

  def isRoyalFlush(cards: Seq[Card]): Boolean = 
    isStraightFlush(cards) && 
    cards.count(c => Seq(Ten, Jack, Queen, King, Ace).contains(c.rank)) == cards.length &&
    cards.length == 5

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
    !isFourOfAKind(cards) && 
    cards.length > 0

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

  def straight(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isStraight) (_.sorted)
  
  def flush(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isFlush) (cs => cs)
  
  def straightFlush(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isStraightFlush) (cs => cs)
  
  def royalFlush(cards: Seq[Card]): Option[Seq[Card]] = filterByPredicate(cards, isRoyalFlush) (cs => cs)

  // def highCardRank(cards: Seq[Card]): Option[Rank] = (commons.suited(cards), highCard(cards)) match {
  //   case (Nil, _) => None
  //   case (_, false) => None
  //   case (cs, true) => Some(cs.sorted.reverse.head.rank)
  // }

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

  def handType(cards: Seq[Card]): Option[PokerHandType] = {
    (isHighCard(cards), isOnePair(cards), isTwoPair(cards), isThreeOfAKind(cards), isStraight(cards), isFlush(cards), isFullHouse(cards), isFourOfAKind(cards), isStraightFlush(cards), isRoyalFlush(cards)) match {
      case (_, _, _, _, _, _, _, _, _, true) => Some(RoyalFlush)
      case (_, _, _, _, _, _, _, _, true, _) => Some(StraightFlush)
      case (_, _, _, _, _, _, _, true, _, _) => Some(FourOfAKind)
      case (_, _, _, _, _, _, true, _, _, _) => Some(FullHouse)
      case (_, _, _, _, _, true, _, _, _, _) => Some(Flush)
      case (_, _, _, _, true, _, _, _, _, _) => Some(Straight)
      case (_, _, _, true, _, _, _, _, _, _) => Some(ThreeOfAKind)
      case (_, _, true, _, _, _, _, _, _, _) => Some(TwoPair)
      case (_, true, _, _, _, _, _, _, _, _) => Some(OnePair)
      case (true, _, _, _, _, _, _, _, _, _) => Some(HighCard)
      case (_, _, _, _, _, _, _, _, _, _) => None
    }
  }

  // TODO: test
  def matched(cards: Seq[Card]): Seq[Card] = handType(cards) match {
    case Some(RoyalFlush) => royalFlush(cards).getOrElse(Nil)
    case Some(StraightFlush) => straightFlush(cards).getOrElse(Nil)
    case Some(FourOfAKind) => fourOfAKind(cards).getOrElse(Nil)
    case Some(FullHouse) => fullHouse(cards).getOrElse(Nil)
    case Some(Flush) => flush(cards).getOrElse(Nil)
    case Some(Straight) => straight(cards).getOrElse(Nil)
    case Some(ThreeOfAKind) => threeOfAKind(cards).getOrElse(Nil)
    case Some(TwoPair) => twoPair(cards).getOrElse(Nil)
    case Some(OnePair) => onePair(cards).getOrElse(Nil)
    case Some(HighCard) => Seq(highCard(cards).get)
    case Some(_) => Nil 
    case None => Nil
  }

  // TODO: test
  def unmatched(cards: Seq[Card]): Seq[Card] = cards.diff(matched(cards))


}