package cards.behaviors

import cards.classes.Card
import cards.classes.Rank._
import cards.classes.Suit._
import cards.classes.actions.Action

trait Textualization {
  // break an action (aka enumeration value) into words:
  // (space-separated, adding an 's' after first word (verb) unless explicitly specified not to do so)
  def words[A <: Enumeration#Value](action: A, letterSVerbSuffix: Boolean): String = {
    var first: Boolean = true
    // first construct sentence by adding spaces before each capital letter which is not the first letter
    val sentence: String = action.toString().flatMap { c => 
      (c, first) match {
        case (_, true) => first = false; c.toLower.toString()
        case (cc, _) if (cc.isUpper) => first = false; " " + c.toLower
        case (_, _) => c.toLower.toString()
      }
    }
    // add 's' suffix after first word only (unless explicitly instructed not to add the 's' suffix)
    val separatedWords: List[String] = sentence.split(" ").toList
    (separatedWords, letterSVerbSuffix) match {
      case (Nil, _) => "" // no words, empty string
      case (x :: Nil, true) => x + "s" // only one word, add 's' after it
      case (x :: xs, true) => x + "s " + xs.mkString(" ") // two or more words, add 's' after first word
      case (_, _) => sentence // not adding 's' after first word, so no need to change anything 
    }
  }

  def words(card: Card): String = (card.rank, card.suit) match {
    case (FaceDown, _) => "face-down card"
    case (LeftBower, _) => "left-bower Joker" 
    case (RightBower, _) => "right-bower Joker"
    case (_, Joker) => "Joker"
    case (_, Unknown) => "unknown"
    case (r, s) => s"$r of $s"
  }

  def words(cards: Seq[Card]): String = cards.map(words(_)).mkString("[", ", ", "]")

  def words[A <: Enumeration#Value](action: Action[A]): String = words(action, true)
  def words[A <: Enumeration#Value](action: Action[A], letterSVerbSuffix: Boolean): String = {
    val actionSentencePrefix: String = ((action.beforeCards, action.beforeTokens) match {
      case (Nil, None) => s"${action.playerId} ${words(action.action, letterSVerbSuffix)}"
      case (_, Some(n)) => s", ${words(action.action, letterSVerbSuffix)}"
      case (cs, _) => s", ${words(action.action, letterSVerbSuffix)}"
    }).replace(" iss ", " is ")

    val actionSentence: String = (action.actionCards, action.actionTokens) match {
      case (Nil, None) => actionSentencePrefix
      case (cs, None) => actionSentencePrefix + s" ${words(cs)}"
      case (Nil, Some(n)) => actionSentencePrefix + s" $n"
      // case (cs, n) => actionSentencePrefix + s" ${words(cs)}, $n"
      case (cs, Some(n)) => actionSentencePrefix + s" ${words(cs)} $n" // jmi
    }
    val preSentence: String = (action.beforeCards, action.beforeTokens) match {
      case (Nil, None) => ""
      case (Nil, Some(n)) => action.playerId + s", starting with $n"
      case (cs, None) => action.playerId + s", starting with ${words(cs)}"
      case (cs, Some(n)) => action.playerId + s", starting with ${words(cs)} and $n"
    }
    val postSentence: String = (action.afterCards, action.afterTokens) match {
      case (Nil, None) => ""
      case (Nil, Some(n)) => s", ending with $n"
      case (cs, None) => s", ending with ${words(cs.head)}"
      case (cs, Some(n)) => s", ending with ${words(cs.head)} and $n"
    }
    preSentence + actionSentence + postSentence 
  }
}