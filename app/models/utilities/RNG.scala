package models.utilities

trait RNG {
  def nextLong: (Long, RNG)
  def nextInt: (Int, RNG)
  def nextBoolean: (Boolean, RNG)
  val boundedPositiveInt: (Int) => (Int, RNG)
}

object RNG {
  case class RandomSeed(seed: Long) extends RNG {
    def nextLong: (Long, RNG) = {
      // Linear Congruential Generator
      val newSeed =  (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // & is bitwise AND
      val nextRng = RandomSeed(newSeed)
      (newSeed, nextRng)
    }
    def nextInt: (Int, RNG) = {
      val (newSeed, nextRng) = nextLong
      val n = (newSeed >>> 16).toInt // >>> is right binary shift with zero fill
      (n, nextRng)
    }
    def nextBoolean: (Boolean, RNG) = {
      nextInt match {
        case (i, r) => ( i % 2 == 0, r)
      }
    }
    val boundedPositiveInt = (upperBound: Int) => {
      def unboundedPositiveInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        if (i < 0) {
          (-(i + 1), r) // if i is negative, then add multiply i+1 by -1
        } else {
          (i, r)
        }
      }
      val (i, r) = unboundedPositiveInt(this)
      ( i % upperBound, r)
    }

  }
}