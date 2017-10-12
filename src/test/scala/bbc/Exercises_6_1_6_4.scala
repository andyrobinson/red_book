package bbc

import org.scalatest.{FunSpec, Matchers}
import RNG._

case class NotRandom(value: Int) extends RNG {
  override def nextInt: (Int, RNG) = (value,this)
}

class Exercises_6_1_6_4 extends FunSpec with Matchers {

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

    it("should do a Double and Int pair") {
      doubleInt(NotRandom(88))._1 shouldBe (89.toDouble/Int.MaxValue,88)
    }

    it("should generate three doubles") {
      val expectedDouble = (47+1).toDouble/Int.MaxValue
      double3(NotRandom(47))._1 shouldBe (expectedDouble,expectedDouble,expectedDouble)
    }

  }

  describe("list of random integers") {
    it("should generate a list") {
      ints(5)(NotRandom(22))._1 shouldBe List(22,22,22,22,22)
      ints(10)(SimpleRNG(456))._1.length shouldBe 10
      ints(6)(SimpleRNG(99))._1 shouldBe List(38090141, 1575376209, 1102671736, -565736979, -407345342, 429642468)
    }
  }

}
