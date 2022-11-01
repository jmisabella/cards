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

  def words[A <: Enumeration#Value](action: Action[A], letterSVerbSuffix: Boolean = true): String = {
    val actionSentencePrefix: String = s"${action.playerId} ${words(action.action, letterSVerbSuffix)}".replace(" iss ", " is ")
    val actionSentence: String = (action.actionCards, action.actionTokens) match {
      case (Nil, 0) => actionSentencePrefix
      case (cs, 0) => actionSentencePrefix + s": ${words(cs)}"
      case (Nil, n) => actionSentencePrefix + s": $n"
      case (cs, n) => actionSentencePrefix + s": ${words(cs)}, $n"
    }
    val preSentence: String = action.beforeCards match {
      case Nil => ""
      case cs => action.playerId + s" starts with ${words(action.beforeCards)}\r\n"
    }
    val postSentence: String = action.afterCards match {
      case Nil => ""
      case cs => s"\r\n${action.playerId} ends with ${words(action.afterCards.head)}"
    }
    preSentence + actionSentence + postSentence 
  }

}