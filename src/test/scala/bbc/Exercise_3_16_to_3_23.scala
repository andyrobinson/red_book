package bbc

import org.scalatest.{FunSpec, Matchers}
import FpList._

class Exercise_3_16_to_3_23 extends FunSpec with Matchers {
  describe("Exercise 3.16 add one") {
    it("should add one to the list") {
      addOne(Nil) shouldBe Nil
      addOne(FpList(4)) shouldBe FpList(5)
      addOne(FpList(4,5,6)) shouldBe FpList(5,6,7)
    }
  }

  describe("Exercise 3.17 double to String") {
    it("should convert to string") {
      doubleToString(Nil) shouldBe Nil
      doubleToString(FpList(1.0D)) shouldBe FpList("1.0")
      doubleToString(FpList(1.0D, 4.78D, 8.111D)) shouldBe FpList("1.0", "4.78", "8.111")
    }
  }

  describe("Exercise 3.18 map") {
    it("should map any function I give it") {
      map(Nil: FpList[Int])(x => x + 1) shouldBe Nil
      map(FpList(3,4,5))(x => x + 1) shouldBe FpList(4,5,6)
    }
  }

  describe("Exercise 3.19 Filter") {
    it("should filter elements which match predicate") {
      filter(Nil: FpList[Int])(_ > 3) shouldBe Nil
      filter(FpList(2,3,4,5,6,1))(_ > 3) shouldBe FpList(4,5,6)
    }

    it("should filter out odd numbers")  {
      filter(FpList(2,3,4,5,6,7,8))(_ % 2 == 0) shouldBe FpList(2,4,6,8)
    }
  }

}
