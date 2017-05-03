package bbc

import org.scalatest.{FunSpec, Matchers}
import FpList._

class Exercise_3_24 extends FunSpec with Matchers {
  describe("ex 3.24 subsequence") {
    it("Nil is a subsequence of anything") {
      hasSubsequence(FpNil, FpNil) shouldBe true
      hasSubsequence(FpList(1,2,3), FpNil) shouldBe true
    }

    it("Anything apart from Nil is not a subsequence of Nil") {
      hasSubsequence(FpNil, FpList(1,2,3)) shouldBe false
    }

    it("Should be a subsequence of iteself") {
      val list = FpList("A", "B")
      hasSubsequence(list, list) shouldBe true
    }

    it("Should really do subsequence") {
      val containingList = FpList(1,2,3,4)
      hasSubsequence(containingList, FpList(1,2)) shouldBe true
      hasSubsequence(containingList, FpList(2,3)) shouldBe true
      hasSubsequence(containingList, FpList(2,3,4)) shouldBe true
      hasSubsequence(containingList, FpList(3,4)) shouldBe true
    }

    it("Should really do not subsequence for lists that don't match") {
      val containingList = FpList(1,2,3,4)
      hasSubsequence(containingList, FpList(1,3)) shouldBe false
      hasSubsequence(containingList, FpList(3,4,5)) shouldBe false
      hasSubsequence(containingList, FpList(2,4)) shouldBe false
    }

  }

}
