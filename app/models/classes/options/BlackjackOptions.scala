package cards.models.classes.options

import play.api.libs.json.{ Json, Format }

// payout ratio if player gets a blackjack
object BlackjackPayout extends Enumeration {
  type BlackjackPayout = Value
  val ThreeToTwo, SixToFive, OneToOne = Value
  implicit val format: Format[BlackjackPayout] = Json.formatEnum(this)
}
// late - if dealer 1st card ace or ten, then make sure no blackjack before surrender is offered to player
// early - player can surrender BEFORE dealer checks for blackjack
object Surrender extends Enumeration {
  type Surrender = Value
  val LateSurrender, EarlySurrender = Value
  implicit val format: Format[Surrender] = Json.formatEnum(this)
}
// S17 - dealer stands on soft 17
// H17 - dealer hits on soft 17
object DealerHitLimit extends Enumeration {
  type DealerHitLimit = Value
  val S17, H17 = Value
  implicit val format: Format[DealerHitLimit] = Json.formatEnum(this)
}

// After first initial split, if split hands initial 2 cards are also same then can these be replit, and if so, how many times
// Zero - no replits allowed
// One, Two, Three, Four - limitation to how many replit hands are allowed
// Unlimitted - no limitation to number of resplit hands allowed
object ResplitLimit extends Enumeration {
  type ResplitLimit = Value
  val Zero, One, Two, Three, Four, Unlimitted = Value
  implicit val format: Format[ResplitLimit] = Json.formatEnum(this)
}

import cards.models.classes.options.BlackjackPayout._
import cards.models.classes.options.Surrender._
import cards.models.classes.options.DealerHitLimit._
import cards.models.classes.options.ResplitLimit._

case class BlackjackOptions(
  deckCount: Int = 1,
  dealerHitLimit: DealerHitLimit = S17,
  payout: BlackjackPayout = ThreeToTwo,
  surrenderType: Option[Surrender] = Some(LateSurrender),
  resplitLimit: ResplitLimit = Unlimitted,
  hitOnSplitAces: Boolean = true,
  resplitOnSplitAces: Boolean = true) {
  
  require(deckCount >= 1 && deckCount <= 8, s"deckCount [$deckCount] is outside of allowed range 1-8")
}
object BlackjackOptions {
  implicit val format: Format[BlackjackOptions] = Json.format[BlackjackOptions]
}
