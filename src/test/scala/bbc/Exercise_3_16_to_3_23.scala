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

  describe("Exercise 3.20 flatMap") {
    it("should fltpMap an empty list to an empty list") {
      flatMap(Nil)(x => FpList(x)) shouldBe Nil
    }

    it("should flatMap a single item to a transformed list with a single item") {
      flatMap(FpList(1))(x => FpList(x.toString)) shouldBe FpList("1")
    }

    it("should flatMap a list to another list") {
      flatMap(FpList(3,2,1))(x => FpList(x.toString)) shouldBe FpList("3", "2", "1")
    }

  }

  describe("Exercise 3.21 Filter implemented using flatMap") {
    it("should filter elements which match predicate") {
      filterByFlatMap(Nil: FpList[Int])(_ > 3) shouldBe Nil
      filterByFlatMap(FpList(2,3,4,5,6,1))(_ > 3) shouldBe FpList(4,5,6)
    }

    it("should filter out odd numbers")  {
      filterByFlatMap(FpList(2,3,4,5,6,7,8))(_ % 2 == 0) shouldBe FpList(2,4,6,8)
    }
  }

  describe("Exercise 3.22 add two lists of numbers together") {
    it("should produce an empty list with two empty lists") {
      zipSum(Nil, Nil) shouldBe Nil
    }

    it("should give the first list if the second list is empty") {
      zipSum(FpList(4,5,6), Nil) shouldBe FpList(4,5,6)
    }

    it("should give the second list if the first list is empty") {
      zipSum(Nil, FpList(4,5,6)) shouldBe FpList(4,5,6)
    }

    it("should add together the corresponding numbers in each list") {
      zipSum(FpList(101, 102, 103), FpList(4,5,6)) shouldBe FpList(105, 107, 109)
    }

    it("should add together lists of different lengths") {
      zipSum(FpList(101, 102, 103), FpList(4,5,6,7,8)) shouldBe FpList(105, 107, 109,7,8)
      zipSum(FpList(101, 102, 103, 104, 105), FpList(4,5,6)) shouldBe FpList(105, 107, 109,104, 105)
    }


  }

}
