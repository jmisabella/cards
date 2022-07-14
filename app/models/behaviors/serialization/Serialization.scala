package cards.models.behaviors.serialization

trait Serialization[A] {
  def parse(json: String): Either[String, Seq[A]]
  def json(items: Seq[A]): String
}
