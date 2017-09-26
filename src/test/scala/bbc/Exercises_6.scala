package bbc

import org.scalatest.{FunSpec, Matchers}
import scala.math.abs

trait RNG {
  def nextInt: (Int, RNG)
}

case class NotRandom(value: Int) extends RNG {
  override def nextInt: (Int, RNG) = (value,this)
}

class Exercises_6 extends FunSpec with Matchers {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (next,rng2) = rng.nextInt
    val safeNext = if (next == Int.MinValue) Int.MaxValue else abs(next)
    (safeNext,rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (next, rng2) = nonNegativeInt(rng)
    ((next+1).toDouble/Int.MaxValue,rng2)
  }

  def intDouble(rng: RNG): ((Int,Double),RNG) = {
    val (nextInt,rng2) = nonNegativeInt(rng)
    val (nextDouble,rng3) = double(rng2)
    ((nextInt,nextDouble),rng3)
  }

  describe("Non negative random numbers 6.1") {
    it("should pass on positive numbers") {
      nonNegativeInt(NotRandom(42))._1 shouldBe 42
    }

    it("should take the absolute values of negative numbers") {
      nonNegativeInt(NotRandom(-5647))._1 shouldBe 5647
    }

    it("should convert MinValue into MaxValue") {
      nonNegativeInt(NotRandom(Int.MinValue))._1 shouldBe Int.MaxValue
    }
  }

  describe("Double random numbers 6.2") {
    it("should convert integer random to double") {
      double(NotRandom(1000))._1 shouldBe (1001).toDouble/Int.MaxValue
    }
  }

  describe("pairs and triples of values 6.3") {
    it("should do an Int and Double pair") {
      intDouble(NotRandom(88))._1 shouldBe (88,89.toDouble/Int.MaxValue)
    }
  }

}
