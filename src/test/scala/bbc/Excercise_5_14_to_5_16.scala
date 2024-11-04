package bbc

import bbc.stream.FpStream
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Excercise_5_14_to_5_16 extends AnyFunSpec with Matchers {

  describe("starts with") {
    it("should reject non prefixes") {
      FpStream(1,2,3).startsWith(FpStream(4,5,6)) shouldBe false
    }

    it("should have some consistent empty stream behaviour") {
      FpStream.empty[Int].startsWith(FpStream(1)) shouldBe false
      FpStream(2).startsWith(FpStream.empty[Int]) shouldBe true
    }

    it("should accept a prefix") {
      FpStream("A","Z","K","L","Q").startsWith(FpStream("A","Z","K")) shouldBe true
    }
  }

  describe("tails") {

    def tailsToList[A](s: FpStream[A]): List[List[A]] =
      s.tails.toList.map(_.toList)

    it("should produce a stream of empty stream") {
      tailsToList(FpStream.empty[String]) shouldBe List(List())
    }

    it("should produce a stream with the element and empty for a singleton Stream") {
      tailsToList(FpStream("A")) shouldBe List(List("A"),List())
    }

    it("should produce a stream with all the tails") {
      tailsToList(FpStream(1,2,3)) shouldBe List(List(1,2,3),List(2,3),List(3),List())
    }
  }

  describe("scanRight") {
    it("should apply a function to list in the list of tails (?)") {
      FpStream(1).scanRight(0)(_ + _).toList shouldBe List(1,0)
    }

    it("should apply a function to the streams in tails") {
      FpStream(3,2,1).scanRight(0)(_ + _).toList shouldBe List(6,3,1,0)
      FpStream("A","B","C").scanRight("")(_ + _).toList shouldBe List("ABC","BC","C","")
    }
  }

}
