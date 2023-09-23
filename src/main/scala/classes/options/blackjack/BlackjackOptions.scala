package cards.classes.options.blackjack

import cards.classes.Rank
import cards.classes.Rank._
import play.api.libs.json.{ Json, Format, JsSuccess, JsValue, JsError }
import cards.classes.options.poker.BettingStructure
import cards.classes.bettingstrategy.BlackjackBettingStrategy
import cards.classes.bettingstrategy.BlackjackBettingStrategy._

// payout ratio if player gets a blackjack
object BlackjackPayout extends Enumeration {
  type BlackjackPayout = Value
  val ThreeToTwo, SixToFive, OneToOne = Value
  implicit val format: Format[BlackjackPayout] = Json.formatEnum(this)

  def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
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

  def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
}

import cards.classes.options.blackjack.BlackjackPayout._
import cards.classes.options.blackjack.DealerHitLimit._

private case class SerializedBlackjackOptions(
  deckCount: String,
  dealerHitLimit: String,
  blackjackPayout: String,
  allowSurrender: String, 
  hitOnSplitAces: String,
  resplitOnSplitAces: String,
  initialBank: String,
  splitLimit: Option[String] = None,
  playerInitialRanks: Option[Seq[String]] = None,
  dealerInitialRanks: Option[Seq[String]] = None,
  initialBettingStrategy: Option[String] = None
)

private object SerializedBlackjackOptions {
  implicit val format: Format[SerializedBlackjackOptions] = Json.format[SerializedBlackjackOptions]
}

// goal is the goal bank amount, upon reaching would cause player to leave table
// playerInitialRanks: optionally specify player's first 2 ranks, for seeing how specific scenarios play out
//    notice that playerInitialRanks length must match dealerInitialRanks length, both must be either 0 or 2 in length
// dealerInitialRanks: optionally specify dealer's first 2 ranks, for seeing how specific scenarios play out
//    notice that playerInitialRanks length must match dealerInitialRanks length, both must be either 0 or 2 in length
case class BlackjackOptions(
  deckCount: Int = 1,
  dealerHitLimit: DealerHitLimit = S17,
  blackjackPayout: BlackjackPayout = ThreeToTwo,
  allowSurrender: Boolean = true, 
  splitLimit: Option[Int] = Some(3),
  hitOnSplitAces: Boolean = true,
  resplitOnSplitAces: Boolean = true,
  initialBank: Int = 2000,
  playerInitialRanks: Seq[Rank] = Nil,
  dealerInitialRanks: Seq[Rank] = Nil,
  initialBettingStrategy: Option[String] = None 
  ) {
  
  require(
    deckCount >= 1 && 
    deckCount <= 8, 
    s"deckCount [$deckCount] is outside of allowed range 1-8")
  require(
    playerInitialRanks.length == dealerInitialRanks.length && 
    (dealerInitialRanks == Nil || dealerInitialRanks.length == 2),
    "Optional initial rank overrides (for player and dealer) must both be either empty or exactly length 2, however " + 
    s"player initial rank length is [${playerInitialRanks.length}] and dealer initial rank length is [${dealerInitialRanks.length}]")

  override def toString(): String = (splitLimit, initialBettingStrategy) match {
    case (None, None) => 
      (Json.obj(
        "deck-count" -> deckCount,
        "dealer-hit-limit" -> dealerHitLimit,
        "blackjack-payout" -> blackjackPayout,
        "allow-surrender" -> allowSurrender,
        "hit-on-split-aces" -> hitOnSplitAces,
        "resplit-on-split-aces" -> resplitOnSplitAces,
        "initial-bank" -> initialBank,
        "initial-player-ranks" -> playerInitialRanks.mkString("[", ",", "]"),
        "initial-dealer-ranks" -> dealerInitialRanks.mkString("[", ",", "]")
      )).toString()
    case (None, Some(bs)) => 
      (Json.obj(
        "deck-count" -> deckCount,
        "dealer-hit-limit" -> dealerHitLimit,
        "blackjack-payout" -> blackjackPayout,
        "allow-surrender" -> allowSurrender,
        "hit-on-split-aces" -> hitOnSplitAces,
        "resplit-on-split-aces" -> resplitOnSplitAces,
        "initial-bank" -> initialBank,
        "initial-player-ranks" -> playerInitialRanks.mkString("[", ",", "]"),
        "initial-dealer-ranks" -> dealerInitialRanks.mkString("[", ",", "]"),
        "initial-betting-strategy" -> bs
      )).toString()
    case (Some(limit), None) => 
      (Json.obj(
        "deck-count" -> deckCount,
        "dealer-hit-limit" -> dealerHitLimit,
        "blackjack-payout" -> blackjackPayout,
        "allow-surrender" -> allowSurrender,
        "split-limit" -> limit,
        "hit-on-split-aces" -> hitOnSplitAces,
        "resplit-on-split-aces" -> resplitOnSplitAces,
        "initial-bank" -> initialBank,
        "initial-player-ranks" -> playerInitialRanks.mkString("[", ",", "]"),
        "initial-dealer-ranks" -> dealerInitialRanks.mkString("[", ",", "]")
      )).toString()
    case (Some(limit), Some(bs)) => 
      (Json.obj(
        "deck-count" -> deckCount,
        "dealer-hit-limit" -> dealerHitLimit,
        "blackjack-payout" -> blackjackPayout,
        "allow-surrender" -> allowSurrender,
        "split-limit" -> limit,
        "hit-on-split-aces" -> hitOnSplitAces,
        "resplit-on-split-aces" -> resplitOnSplitAces,
        "initial-bank" -> initialBank,
        "initial-player-ranks" -> playerInitialRanks.mkString("[", ",", "]"),
        "initial-dealer-ranks" -> dealerInitialRanks.mkString("[", ",", "]"),
        "initial-betting-strategy" -> bs
      )).toString()
  }
}
object BlackjackOptions {
  implicit val format: Format[BlackjackOptions] = Json.format[BlackjackOptions]

  def apply(serialized: SerializedBlackjackOptions): BlackjackOptions = {
    BlackjackOptions(
      deckCount = serialized.deckCount.toInt,
      dealerHitLimit = DealerHitLimit.withNameOpt(serialized.dealerHitLimit).getOrElse(throw new IllegalStateException(s"Cannot deserialize String value [${serialized.dealerHitLimit}] to a DealerHitLimit. Expected values: [S17, H17]")),
      blackjackPayout = BlackjackPayout.withNameOpt(serialized.blackjackPayout).getOrElse(throw new IllegalStateException(s"Cannot deserialize String value [${serialized.blackjackPayout}] to a BlackjackPayout. Expected values: [OneToOne, ThreeToTwo, SixToFive]")),
      allowSurrender = serialized.allowSurrender.toBoolean,
      splitLimit = serialized.splitLimit match {
        case Some(s) => Some(s.toInt)
        case _ => None
      },
      hitOnSplitAces = serialized.hitOnSplitAces.toBoolean,
      resplitOnSplitAces = serialized.resplitOnSplitAces.toBoolean,
      initialBank = serialized.initialBank.toInt,
      playerInitialRanks = serialized.playerInitialRanks.getOrElse(Nil).map(Rank.withNameOpt(_).get),
      dealerInitialRanks = serialized.dealerInitialRanks.getOrElse(Nil).map(Rank.withNameOpt(_).get),
      initialBettingStrategy = serialized.initialBettingStrategy match {
        case Some(bs) => Some(bs)
        case None => None
      }
    )
  }

  def apply(json: String): BlackjackOptions = {
    def replacements(json: String): String = json  
      .replace("deck-count", "deckCount")
      .replace("dealer-hit-limit", "dealerHitLimit")
      .replace("blackjack-payout", "blackjackPayout")
      .replace("allow-surrender", "allowSurrender")
      .replace("split-limit", "splitLimit")
      .replace("hit-on-split-aces", "hitOnSplitAces")
      .replace("resplit-on-split-aces", "resplitOnSplitAces")
      .replace("initial-bank", "initialBank")
      .replace("initial-player-ranks", "playerInitialRanks")
      .replace("initial-dealer-ranks", "dealerInitialRanks")
      .replace("initial-betting-strategy", "initialBettingStrategy")

    Json.parse(replacements(json)).validate[SerializedBlackjackOptions] match {
      case JsSuccess(opts, _) => BlackjackOptions(opts)
      case _ => Json.parse(replacements(json)).validate[BlackjackOptions] match {
        case JsSuccess(opts, _) => opts
        case e => throw new IllegalArgumentException(s"Error occurred deserializing json [${json}] to a SerializedBlackjackOptions object: " + e.toString())
      }
    }
  }
}
