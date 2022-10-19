package behaviors

import cards.classes.actions.Action

trait Textualization {

  // break an action (aka enumeration value) into words:
  // (space-separated, adding an 's' after first word (verb) unless explicitly specified not to do so)
  def words[A <: Enumeration#Value](action: A, addLetterS: Boolean = true): String = {
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
    (separatedWords, addLetterS) match {
      case (Nil, _) => "" // no words, empty string
      case (x :: Nil, true) => x + "s" // only one word, add 's' after it
      case (x :: xs, true) => x + "s " + xs.mkString(" ") // two or more words, add 's' after first word
      case (_, _) => sentence // if not adding 's' after first word, then no need to change anything 
    }
  }

  //  def words[A <: Enumeration#Value](sentence: Action[A]): String = {
  //   sentence.playerId + " " + words(sentence.action) + 
  //  }


}
