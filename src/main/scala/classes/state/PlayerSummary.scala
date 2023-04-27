package cards.classes.state

import cards.classes.actions.{ Action, BlackjackAction }
import play.api.libs.json.{ Json, Format }

// game summary
case class PlayerSummary(id: String, bank: Int, highestBank: Int, rounds: Int) {
  override def toString(): String = {
    (Json.obj(
      "id" -> id,
      "bank" -> bank,
      "highestBank" -> highestBank,
      "rounds" -> rounds
    )).toString()
  }
}
object PlayerSummary {
  def apply[A <: PlayerState](player: A): PlayerSummary = PlayerSummary(player.id, player.bank, player.highestBank, player.rounds)
  implicit val format: Format[PlayerSummary] = Json.format[PlayerSummary]
}
case class PlayerSummaries(players: Seq[PlayerSummary]) {
  def apply[A <: PlayerState](players: Seq[A]): PlayerSummaries = PlayerSummaries(players.map(PlayerSummary(_)))
  override def toString(): String = (players.map(_.toString())).mkString("""{"players":[""", "," ,"]}") 
}
object PlayerSummaries {
  implicit val format1: Format[PlayerSummary] = Json.format[PlayerSummary]
  implicit val format2: Format[PlayerSummaries] = Json.format[PlayerSummaries]
}