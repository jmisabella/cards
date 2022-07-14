package cards.models.behaviors.serialization

import cards.models.behaviors.serialization.Serialization
import cards.models.classes.{ Card, Cards }
import play.api.libs.json. { Json, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

trait CardSerialization extends Serialization[Card] {
  override def parse(json: String): Either[String, Seq[Card]] = try {
    Json.parse(json).validate[Cards] match {
      case JsSuccess(cs, _) => Right(cs.cards)
      case e => Left(s"Error occurred: ${e.toString()}")
    }
  } catch {
    case e: JsonParseException => Left(e.getMessage())
  }

  override def json(items: Seq[Card]): String = Cards(items).toString()
}
