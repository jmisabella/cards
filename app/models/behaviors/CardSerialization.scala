package cards.models.behaviors

import cards.models.classes.{ Card, Cards }
import play.api.libs.json. { Json, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

trait CardSerialization {
  def parse(json: String): Either[String, Seq[Card]] = try {
    Json.parse(json).validate[Cards] match {
      case JsSuccess(cs, _) => Right(cs.cards)
      case e => Left(s"Error occurred: ${e.toString()}")
    }
  } catch {
    case e: JsonParseException => Left(e.getMessage())
  }

  def json(cards: Seq[Card]): String = Cards(cards).toString()
}
