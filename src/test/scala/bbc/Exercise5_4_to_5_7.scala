package bbc

import bbc.stream.FpStream
import org.scalatest.{FunSpec, Matchers}

class Exercise5_4_to_5_7 extends FunSpec with Matchers {

  describe("5.4 ForAll") {
    it("should return true for an empty stream") {
      val empty = FpStream.empty[String]
      empty.forAll(x => x.contains("blah")) shouldBe true
    }

    it("should return true for a stream with one element which satisfies the condition") {
      val empty = FpStream(4, 5, 6)
      empty.forAll(_ > 2) shouldBe true
    }

    it("should return false for a stream when at least one element does not satisfy the condition") {
      val empty = FpStream(4, 5, 6, 2)
      empty.forAll(_ > 2) shouldBe false
    }

    it("Ex 5.5 should take elements while the condition is true") {
      val stream = FpStream("A", "B", "C")

      FpStream.empty[String].takeWhileFR(x => true) shouldBe FpStream.empty[String]
      stream.takeWhileFR(x => true).toList shouldBe stream.toList
      stream.takeWhileFR(x => (x == "A" || x == "B")).toList shouldBe List("A" , "B")

    }


  }
}
