package bbc

import org.scalatest.{FunSpec, Matchers}

class ListSpec extends FunSpec with Matchers {
  describe("list") {

    def addup(list: List[Int]): Int = list match {
      case Nil => 0
      case head :: tail => head + addup(tail)
    }

    it("should add up a list of numbers") {
      addup(List()) shouldBe 0
      addup(List(1)) shouldBe 1
      addup(List(1,2,3)) shouldBe 6
    }
  }
}
