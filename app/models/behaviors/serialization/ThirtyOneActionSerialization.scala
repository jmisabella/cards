package cards.models.behaviors.serialization

import cards.models.behaviors.serialization.Serialization
import cards.models.classes.actions.{ Action, Actions }
import cards.models.classes.actions.ThirtyOneAction._
import play.api.libs.json. { Json, JsSuccess, Format }
import com.fasterxml.jackson.core.JsonParseException

trait ThirtyOneActionSerialization extends Serialization[Action[ThirtyOneAction]] {
  override def parse(json: String): Either[String, Seq[Action[ThirtyOneAction]]] = try {
    Json.parse(json).validate[Actions[ThirtyOneAction]] match {
      case JsSuccess(as, _) => Right(as.actions)
      case e => Left(s"Error occurred: ${e.toString()}")
    }
  } catch {
    case e: JsonParseException => Left(e.getMessage())
  }

  override def json(items: Seq[Action[ThirtyOneAction]]): String = Actions(items).toString()
}
