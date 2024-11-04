package bbc

import bbc.stream.FpStream
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Exercise5_4_to_5_7 extends AnyFunSpec with Matchers {

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

    describe("Ex 5.6 head option") {
      it("should return None if the stream is empty") {
        val stream = FpStream.empty[Int]
        stream.headOption shouldBe None
      }

      it("should return Some(head) if the stream has a head value") {
        val stream = FpStream("A")
        stream.headOption shouldBe Some("A")
      }

      it("should act lazily") {
        def tailError: FpStream[String] = ???

        val stream = FpStream.cons("B", tailError )
        stream.headOption shouldBe Some("B")
      }
    }

    describe("Ex 5.7 methods in terms of FoldRight") {
      it("should implement map") {
        val stream = FpStream(1,2,3)
        stream.map(x => x + 1).toList shouldBe List(2,3,4)
      }

      it("should implement filter") {
        val stream = FpStream(5,6,7)
        stream.filter(x => x < 7).toList shouldBe List(5,6)
        stream.filter(x => x > 10).toList shouldBe List.empty[Int]
        FpStream.empty[String].filter(x => x == "A") shouldBe FpStream.empty[String]
      }

      it("should implement append") {
        val stream = FpStream("A", "B", "C")
        val stream2 = FpStream("D", "E")
        stream.append(stream2).toList shouldBe List("A","B","C","D","E")
      }

      it("should implement flapMap") {
        val stream = FpStream(1,2,3)
        stream.flatMap(x => FpStream(x + 1)).toList shouldBe List(2,3,4)
      }
    }
  }
}
