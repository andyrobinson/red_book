package bbc

import org.scalatest.{FunSpec, Matchers}

import FpList._

class Exercise_3_7_to_3_15 extends FunSpec with Matchers {
  describe("Exercise 3.8 foldRight") {
    it("should reverse the list?") {
      val result = foldRight(FpList(1, 2, 3), FpNil: FpList[Int])(Cons(_, _))
      result shouldBe FpList(1, 2, 3)
    }
  }

  describe("Exercise 3.9 length") {
    def length[A](list: FpList[A]): Int =
      foldRight(list, 0)((_, acc) => acc + 1)

    it("should return zero for the empty list") {
      length(FpNil) shouldBe 0
    }

    it("should return length of non empty list") {
      length(FpList("A", "B", "Z")) shouldBe 3
    }

  }

  describe("Exercise 3.10 foldLeft") {

    it("should return base value for empty list") {
      foldLeft(FpNil: FpList[Int], 99)(_ + _) shouldBe 99
    }

    it("should apply the function successively from the left") {
      val result = foldLeft(FpList("P", "Q", "R"), "")((str, acc) => str + acc)
      result shouldBe "PQR"
    }

    it("should reverse the list") {
      val result = foldLeft(FpList(1, 2, 3), FpNil: FpList[Int])((acc, value) => Cons(value, acc))

      result shouldBe (FpList(3, 2, 1))
    }
  }

  describe("Exercise 3.11 sum and product") {

    it("should produce zero for the empty list") {
      sum(FpNil) shouldBe 0
    }

    it("should add up a list of numbers") {
      val list = FpList(5, 6, 7)
      sum(list) shouldBe 18
    }

    it("should return one for the empty list") {
      product(FpNil) shouldBe 1
    }

    it("should return the product of a list of integers") {
      val list = FpList(5, 6, 7)
      product(list) shouldBe 210
    }
  }

  describe("Exercise 3.12 reverse") {
    it("should reverse the list") {
      val result = foldLeft(FpList(1, 2, 3), FpNil: FpList[Int])((acc, value) => Cons(value, acc))

      result shouldBe (FpList(3, 2, 1))
    }

  }

  describe("Exercise 3.13 foldRight in terms of Fold Left") {

    it("should return 0 for the empty list") {
      foldRightFromLeft(FpNil: FpList[Int], 0)(_ + _) shouldBe 0
    }

    it("should join characters") {
      foldRightFromLeft(FpList("A", "B", "C"), "")(_ + _) shouldBe "ABC"
    }

    it("should preserve the list") {
      val result = foldRightFromLeft(FpList(1, 2, 3), FpNil: FpList[Int])(Cons(_, _))
      result shouldBe FpList(1, 2, 3)
    }
  }

  describe("Exercise 3.13 foldLeft in terms of foldRight") {

    it("should return base value for empty list") {
      foldLeftFromRight(FpNil: FpList[Int], 99)(_ + _) shouldBe 99
    }

    it("should apply the function successively from the left") {
      val result = foldLeftFromRight(FpList("P", "Q", "R"), "")((str, acc) => str + acc)
      result shouldBe "PQR"
    }

    it("should reverse the list") {
      val result = foldLeftFromRight(FpList(1, 2, 3), FpNil: FpList[Int])((acc, value) => Cons(value, acc))

      result shouldBe (FpList(3, 2, 1))
    }
  }

  describe("Exercise 3.14 append using foldLeft or foldRight") {

    it("should return first list if second list is empty") {
      append(FpList(7, 6, 5), FpNil) shouldBe (FpList(7, 6, 5))
    }

    it("should append second list to first") {
      append(FpList("A", "Z"), FpList("P", "Q")) shouldBe FpList("A", "Z", "P", "Q")
    }

    it("should return second list if first list is empty") {
      append(FpNil, FpList(9,8,7)) shouldBe FpList(9,8,7)
    }

  }

  describe("Exercise 3.15 should flatten a list of lists") {

    it("should flatten a list of empty lists into a single Nil") {
      flatten(FpList(FpNil, FpNil, FpNil)) shouldBe FpNil
    }

    it("should flatten a list of one list into that list") {
      flatten(FpList(FpList(1,2,3))) shouldBe FpList(1,2,3)
    }

    it("should join and flatten two lists ") {
      flatten(FpList(FpList(1,2,3), FpList(4,5,6))) shouldBe FpList(1,2,3,4,5,6)
    }

    it("should join and flatten multiple lists ") {
      flatten(FpList(FpList(1,2,3), FpNil, FpList(4,5,6), FpNil, FpList(99))) shouldBe FpList(1,2,3,4,5,6,99)
      flatten(FpList(FpList(1,2,3,6,7,8), FpNil, FpList(4,5,6), FpNil, FpList(99))) shouldBe FpList(1,2,3,6,7,8,4,5,6,99)
    }


  }
}