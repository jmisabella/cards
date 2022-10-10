package cards.behaviors.serialization

import cards.behaviors.serialization.Serialization
import cards.classes.actions.{ Action, Actions }
import cards.classes.actions.BlackjackAction._
// import play.api.libs.json. { Json, JsSuccess, Format }
import com.fasterxml.jackson.core.JsonParseException
import cards.utilities.GsonListAdapter


trait BlackjackActionSerialization extends Serialization[Action[BlackjackAction]] {
  override def parse(json: String): Either[String, Seq[Action[BlackjackAction]]] = try {
    Right(GsonListAdapter.fromJson[Seq[Action[BlackjackAction]]](json))
    // Json.parse(json).validate[Actions[BlackjackAction]] match {
    //   case JsSuccess(as, _) => Right(as.actions)
    //   case e => Left(s"Error occurred: ${e.toString()}")
    // }
  } catch {
    case e: JsonParseException => Left(e.getMessage())
  }

  override def json(items: Seq[Action[BlackjackAction]]): String = Actions(items).toString()
}
