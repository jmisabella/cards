package models.utilities

import models.utilities.RNG
import org.scalatest.flatspec.AnyFlatSpec

class RNGSpec extends AnyFlatSpec {

  "RNG" should "give consistent integer results for a given seed 743897" in {
    val seed = 743897
    val r1: RNG = RNG.RandomSeed(seed)
    val expectedInt1: Int = r1.nextInt._1
    val expectedInt2: Int = r1.nextInt._2.nextInt._1
    val expectedInt3: Int = r1.nextInt._2.nextInt._2.nextInt._1
    val expectedInt4: Int = r1.nextInt._2.nextInt._2.nextInt._2.nextInt._1
    val expectedInt5: Int = r1.nextInt._2.nextInt._2.nextInt._2.nextInt._2.nextInt._1
    val r2: RNG = RNG.RandomSeed(seed)
    val actualInt1: Int = r2.nextInt._1
    val actualInt2: Int = r2.nextInt._2.nextInt._1
    val actualInt3: Int = r2.nextInt._2.nextInt._2.nextInt._1
    val actualInt4: Int = r2.nextInt._2.nextInt._2.nextInt._2.nextInt._1
    val actualInt5: Int = r2.nextInt._2.nextInt._2.nextInt._2.nextInt._2.nextInt._1
    assert(expectedInt1 == actualInt1)
    assert(expectedInt2 == actualInt2)
    assert(expectedInt3 == actualInt3)
    assert(expectedInt4 == actualInt4)
    assert(expectedInt5 == actualInt5)
  }

  it should "give consistent long integer results for a given seed 57" in {
    val seed = 57
    val r1: RNG = RNG.RandomSeed(seed)
    val expectedLong1: Long = r1.nextLong._1
    val expectedLong2: Long = r1.nextLong._2.nextLong._1
    val expectedLong3: Long = r1.nextLong._2.nextLong._2.nextLong._1
    val expectedLong4: Long = r1.nextLong._2.nextLong._2.nextLong._2.nextLong._1
    val expectedLong5: Long = r1.nextLong._2.nextLong._2.nextLong._2.nextLong._2.nextLong._1
    val r2: RNG = RNG.RandomSeed(seed)
    val actualLong1: Long = r2.nextLong._1
    val actualLong2: Long = r2.nextLong._2.nextLong._1
    val actualLong3: Long = r2.nextLong._2.nextLong._2.nextLong._1
    val actualLong4: Long = r2.nextLong._2.nextLong._2.nextLong._2.nextLong._1
    val actualLong5: Long = r2.nextLong._2.nextLong._2.nextLong._2.nextLong._2.nextLong._1
    assert(expectedLong1 == actualLong1)
    assert(expectedLong2 == actualLong2)
    assert(expectedLong3 == actualLong3)
    assert(expectedLong4 == actualLong4)
    assert(expectedLong5 == actualLong5)
  }

  it should "give consistent boolean results for a given seed 80" in {
    val seed = 80
    val r1: RNG = RNG.RandomSeed(seed)
    val expected1: Boolean = r1.nextBoolean._1
    val expected2: Boolean = r1.nextBoolean._2.nextBoolean._1
    val expected3: Boolean = r1.nextBoolean._2.nextBoolean._2.nextBoolean._1
    val expected4: Boolean = r1.nextBoolean._2.nextBoolean._2.nextBoolean._2.nextBoolean._1
    val expected5: Boolean = r1.nextBoolean._2.nextBoolean._2.nextBoolean._2.nextBoolean._2.nextBoolean._1
    val r2: RNG = RNG.RandomSeed(seed)
    val actual1: Boolean = r2.nextBoolean._1
    val actual2: Boolean = r2.nextBoolean._2.nextBoolean._1
    val actual3: Boolean = r2.nextBoolean._2.nextBoolean._2.nextBoolean._1
    val actual4: Boolean = r2.nextBoolean._2.nextBoolean._2.nextBoolean._2.nextBoolean._1
    val actual5: Boolean = r2.nextBoolean._2.nextBoolean._2.nextBoolean._2.nextBoolean._2.nextBoolean._1
    assert(expected1 == actual1)
    assert(expected2 == actual2)
    assert(expected3 == actual3)
    assert(expected4 == actual4)
    assert(expected5 == actual5)
  }

  it should "provide consistent bounded positive integers with upper bound 200 and for a given seed 5431" in {
    val seed = 5431
    val upperBound = 200
    var r1: RNG = RNG.RandomSeed(seed)
    var r2: RNG = RNG.RandomSeed(seed) 
    for (_ <- 0 until 99999) {
      val (expectedResult, nextr1)  = r1.boundedPositiveInt(upperBound)
      val (actualResult, nextr2)  = r2.boundedPositiveInt(upperBound)
      assert(expectedResult >= 0)
      assert(expectedResult < upperBound)
      assert(expectedResult == actualResult)
      r1 = nextr1
      r2 = nextr2
    }
  }

}
