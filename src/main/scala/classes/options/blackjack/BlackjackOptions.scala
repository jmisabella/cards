package cards.classes.options.blackjack

import play.api.libs.json.{ Json, Format, JsSuccess }

// payout ratio if player gets a blackjack
object BlackjackPayout extends Enumeration {
  type BlackjackPayout = Value
  val ThreeToTwo, SixToFive, OneToOne = Value
  implicit val format: Format[BlackjackPayout] = Json.formatEnum(this)
}
// // late - if dealer 1st card ace or ten, then make sure no blackjack before surrender is offered to player
// // early - player can surrender BEFORE dealer checks for blackjack
// object Surrender extends Enumeration {
//   type Surrender = Value
//   val LateSurrender, EarlySurrender = Value
//   implicit val format: Format[Surrender] = Json.formatEnum(this)
// }
// S17 - dealer stands on soft 17
// H17 - dealer hits on soft 17
object DealerHitLimit extends Enumeration {
  type DealerHitLimit = Value
  val S17, H17 = Value
  implicit val format: Format[DealerHitLimit] = Json.formatEnum(this)
}

import cards.classes.options.blackjack.BlackjackPayout._
import cards.classes.options.blackjack.DealerHitLimit._

// goal is the goal bank amount, upon reaching would cause player to leave table
case class BlackjackOptions(
  deckCount: Int = 1,
  dealerHitLimit: DealerHitLimit = S17,
  blackjackPayout: BlackjackPayout = ThreeToTwo,
  allowSurrender: Boolean = true, 
  splitLimit: Option[Int] = Some(3),
  hitOnSplitAces: Boolean = true,
  resplitOnSplitAces: Boolean = true) {
  
  require(deckCount >= 1 && deckCount <= 8, s"deckCount [$deckCount] is outside of allowed range 1-8")

  override def toString(): String = splitLimit match {
    case None => 
      (Json.obj(
        "deck-count" -> deckCount,
        "dealer-hit-limit" -> dealerHitLimit,
        "blackjack-payout" -> blackjackPayout,
        "allow-surrender" -> allowSurrender,
        "hit-on-split-aces" -> hitOnSplitAces,
        "resplit-on-split-aces" -> resplitOnSplitAces,
      )).toString()
    case Some(limit) => 
      (Json.obj(
        "deck-count" -> deckCount,
        "dealer-hit-limit" -> dealerHitLimit,
        "blackjack-payout" -> blackjackPayout,
        "allow-surrender" -> allowSurrender,
        "split-limit" -> limit,
        "hit-on-split-aces" -> hitOnSplitAces,
        "resplit-on-split-aces" -> resplitOnSplitAces,
      )).toString()
  }
}
object BlackjackOptions {
  implicit val format: Format[BlackjackOptions] = Json.format[BlackjackOptions]

  def apply(json: String): BlackjackOptions = {
    val replacements: String = json
      .replace("deck-count", "deckCount")
      .replace("dealer-hit-limit", "dealerHitLimit")
      .replace("blackjack-payout", "blackjackPayout")
      .replace("allow-surrender", "allowSurrender")
      .replace("split-limit", "splitLimit")
      .replace("hit-on-split-aces", "hitOnSplitAces")
      .replace("resplit-on-split-aces", "resplitOnSplitAces")
    Json.parse(replacements).validate[BlackjackOptions] match {
      case JsSuccess(opts, _) => opts
      case e => throw new IllegalArgumentException(s"Error occurred deserializing json [$replacements] to a BlackjackOptions object: " + e.toString())
    }
  }
}
