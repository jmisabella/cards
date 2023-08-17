package cards.classes.options

import cards.classes.options.blackjack.BlackjackOptions
import cards.classes.Rank._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen
import cards.classes.options.blackjack.DealerHitLimit
import cards.classes.options.blackjack.BlackjackPayout
import play.api.libs.json.Json

class BlackjackOptionsSpec extends AnyFlatSpec with GivenWhenThen {
  "BlackjackOptions" should "allow between 1 and 8 decks to be used but disallow any number of decks outside of the allowed range 1-8" in {
    BlackjackOptions(1)
    BlackjackOptions(2)
    BlackjackOptions(3)
    BlackjackOptions(4)
    BlackjackOptions(5)
    BlackjackOptions(6)
    BlackjackOptions(7)
    BlackjackOptions(8)
    an [IllegalArgumentException] should be thrownBy BlackjackOptions(0)
    an [IllegalArgumentException] should be thrownBy BlackjackOptions(9)
  }

  it should "be able to serialize as a json string" in {
    Given("a blackjack options with deck count 1, dealer hit limit S17, 1:1 blackjack payout, split limit 3, and allowance of " + 
      " surrender, hit on split aces, and resplit on split aces")
    val options = BlackjackOptions(1, DealerHitLimit.S17, BlackjackPayout.OneToOne, true, Some(3), true, true)
    When("converting the blackjack options object to a string representation")
    val result: String = options.toString() 
    Then("the result is in JSON format")
    val expected: String = 
      (Json.obj(
        "deck-count" -> options.deckCount,
        "dealer-hit-limit" -> options.dealerHitLimit,
        "blackjack-payout" -> options.blackjackPayout,
        "allow-surrender" -> options.allowSurrender,
        "split-limit" -> options.splitLimit.get,
        "hit-on-split-aces" -> options.hitOnSplitAces,
        "resplit-on-split-aces" -> options.resplitOnSplitAces,
        "initial-bank" -> 2000,
        "initial-player-ranks" -> options.playerInitialRanks.mkString("[", ",", "]"),
        "initial-dealer-ranks" -> options.dealerInitialRanks.mkString("[", ",", "]")
      )).toString()
    result should equal (expected)
  }

  it should "be able to deserialize a json string with split-limit specified to an object" in {
    Given("JSON string which includes the following fields: deck count 1, dealer hit limit S17, 1:1 blackjack payout, split limit 3, and allowances of " + 
      " surrender, hit on split aces, and resplit on split aces")
    val json: String = 
      (Json.obj(
        "deck-count" -> 1,
        "dealer-hit-limit" -> "S17",
        "blackjack-payout" -> "OneToOne",
        "allow-surrender" -> true,
        "split-limit" -> 3,
        "hit-on-split-aces" -> true,
        "resplit-on-split-aces" -> true,
        "initial-bank" -> 2000,
        "initial-player-ranks" -> Nil,
        "initial-dealer-ranks" -> Nil
      )).toString()
    When("converting the JSON string into a blackjack options object")
    val result = BlackjackOptions(json)
    Then("the object should be created as expected without any deserialization errors")
    result.splitLimit should equal (Some(3))
    result.deckCount should equal (1)
    result.blackjackPayout should equal (BlackjackPayout.OneToOne)
    result.dealerHitLimit should equal (DealerHitLimit.S17)
    result.allowSurrender shouldBe (true)
    result.hitOnSplitAces shouldBe (true)
    result.resplitOnSplitAces shouldBe (true)
  }
  
  it should "be able to deserialize a json string with split-limit specified and with all values as type String to an object" in {
    Given("JSON string which includes the following fields: deck count 1, dealer hit limit S17, 1:1 blackjack payout, split limit 3, and allowances of " + 
      " surrender, hit on split aces, and resplit on split aces")
    val json: String = 
      (Json.obj(
        "deck-count" -> 1.toString(),
        "dealer-hit-limit" -> "S17",
        "blackjack-payout" -> "OneToOne",
        "allow-surrender" -> true.toString(),
        "split-limit" -> 3.toString(),
        "hit-on-split-aces" -> true.toString(),
        "resplit-on-split-aces" -> true.toString(),
        "initial-bank" -> 2000.toString(),
        "initial-player-ranks" -> Nil,
        "initial-dealer-ranks" -> Nil
      )).toString()
    When("converting the JSON string into a blackjack options object")
    val result = BlackjackOptions(json)
    Then("the object should be created as expected without any deserialization errors")
    result.splitLimit should equal (Some(3))
    result.deckCount should equal (1)
    result.blackjackPayout should equal (BlackjackPayout.OneToOne)
    result.dealerHitLimit should equal (DealerHitLimit.S17)
    result.allowSurrender shouldBe (true)
    result.hitOnSplitAces shouldBe (true)
    result.resplitOnSplitAces shouldBe (true)
  }

  it should "be able to deserialize a json string with no split-limit specified to an object" in {
    Given("JSON string which includes the following fields: deck count 1, dealer hit limit H17, 3:2 blackjack payout, and allowances of " + 
      " surrender, hit on split aces, and resplit on split aces")
    val json: String = 
      (Json.obj(
        "deck-count" -> 1.toString(),
        "dealer-hit-limit" -> "H17",
        "blackjack-payout" -> "ThreeToTwo",
        "allow-surrender" -> true.toString(),
        "hit-on-split-aces" -> true.toString(),
        "resplit-on-split-aces" -> true.toString(),
        "initial-bank" -> 2000.toString()//,
        // "initial-player-ranks" -> Nil.mkString("[", ",", "]"),
        // "initial-dealer-ranks" -> Nil.mkString("[", ",", "]")
      )).toString()
    When("converting the JSON string into a blackjack options object")
    val result = BlackjackOptions(json)
    Then("the object should be created as expected without any deserialization errors")
    result.splitLimit should equal (None)
    result.deckCount should equal (1)
    result.blackjackPayout should equal (BlackjackPayout.ThreeToTwo)
    result.dealerHitLimit should equal (DealerHitLimit.H17)
    result.allowSurrender shouldBe (true)
    result.hitOnSplitAces shouldBe (true)
    result.resplitOnSplitAces shouldBe (true)
  }

  it should "be able to deserialize a json string with no split-limit specified and with all values as type String to an object" in {
    Given("JSON string which includes the following fields: deck count \"1\", dealer hit limit H17, 3:2 blackjack payout, and allowances of " + 
      " surrender, hit on split aces, and resplit on split aces")
    val json: String = 
      (Json.obj(
        "deck-count" -> 1.toString(),
        "dealer-hit-limit" -> "H17",
        "blackjack-payout" -> "ThreeToTwo",
        "allow-surrender" -> true.toString(),
        "hit-on-split-aces" -> true.toString(),
        "resplit-on-split-aces" -> true.toString(),
        "initial-bank" -> 2000.toString()
      )).toString()
    When("converting the JSON string into a blackjack options object")
    val result = BlackjackOptions(json)
    Then("the object should be created as expected without any deserialization errors")
    result.splitLimit should equal (None)
    result.deckCount should equal (1)
    result.blackjackPayout should equal (BlackjackPayout.ThreeToTwo)
    result.dealerHitLimit should equal (DealerHitLimit.H17)
    result.allowSurrender shouldBe (true)
    result.hitOnSplitAces shouldBe (true)
    result.resplitOnSplitAces shouldBe (true)
  }

  it should "be able to deserialize a json string specifying both player and dealer initial ranks" in {
    Given("JSON string which includes the following fields: deck count 1, dealer hit limit H17, 3:2 blackjack payout, and allowances of " + 
      " surrender, hit on split aces, resplit on split aces, player initial ranks of Queen/Queen, and dealer initial ranks of King/Two")
    val json: String = 
      (Json.obj(
        "deck-count" -> 1.toString(),
        "dealer-hit-limit" -> "H17",
        "blackjack-payout" -> "ThreeToTwo",
        "allow-surrender" -> true.toString(),
        "hit-on-split-aces" -> true.toString(),
        "resplit-on-split-aces" -> true.toString(),
        "initial-bank" -> 2000.toString(),
        "initial-player-ranks" -> Seq(Queen, Queen),
        "initial-dealer-ranks" -> Seq(King, Two)
      )).toString()
    When("converting the JSON string into a blackjack options object: " + json)
    val result = BlackjackOptions(json)
    Then("the object should be created as expected without any deserialization errors")
    result.splitLimit should equal (None)
    result.deckCount should equal (1)
    result.blackjackPayout should equal (BlackjackPayout.ThreeToTwo)
    result.dealerHitLimit should equal (DealerHitLimit.H17)
    result.allowSurrender shouldBe (true)
    result.hitOnSplitAces shouldBe (true)
    result.resplitOnSplitAces shouldBe (true)
    result.playerInitialRanks should have length (2)
    result.playerInitialRanks should contain (Queen)
    result.playerInitialRanks.head should equal (Queen)
    result.playerInitialRanks.tail.head should equal (Queen)
    result.dealerInitialRanks should have length (2)
    result.dealerInitialRanks.head should equal (King)
    result.dealerInitialRanks.tail.head should equal (Two)
  }

  it should "be able to deserialize a json string specifying both player and dealer initial ranks, version 2" in {
    Given("JSON string which includes the following fields: deck count 1, dealer hit limit S17, 3:2 blackjack payout, and allowances of " + 
      " surrender, hit on split aces, resplit on split aces, player initial ranks of Queen/Queen, and dealer initial ranks of King/Ace")
    val json: String = """{"dealerHitLimit":"S17","blackjackPayout":"ThreeToTwo","deckCount":"1","splitLimit":"3","allowSurrender":"true","hitOnSplitAces":"true","resplitOnSplitAces":"true","initialBank":"2000","playerInitialRanks":["Queen","Queen"],"dealerInitialRanks":["King","Ace"]}""";
    When("converting the JSON string into a blackjack options object")
    val result = BlackjackOptions(json)
    Then("the object should be created as expected without any deserialization errors")
    result.splitLimit should equal (Some(3))
    result.deckCount should equal (1)
    result.blackjackPayout should equal (BlackjackPayout.ThreeToTwo)
    result.dealerHitLimit should equal (DealerHitLimit.S17)
    result.allowSurrender shouldBe (true)
    result.hitOnSplitAces shouldBe (true)
    result.resplitOnSplitAces shouldBe (true)
    result.playerInitialRanks should have length (2)
    result.playerInitialRanks should contain (Queen)
    result.playerInitialRanks.head should equal (Queen)
    result.playerInitialRanks.tail.head should equal (Queen)
    result.dealerInitialRanks should have length (2)
    result.dealerInitialRanks.head should equal (King)
    result.dealerInitialRanks.tail.head should equal (Ace)
  }

}