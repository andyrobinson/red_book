package bbc

import bbc.stream.FpStream
import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

class Exercise5_1_to_5_3 extends FunSpec with Matchers {

  def infiniteIntegers(start: Int): FpStream[Int] = {
    FpStream.cons(start, infiniteIntegers(start + 1))
  }

  it("Ex 5.1 should convert an empty stream to an empty list") {
    val a: FpStream[Int] = FpStream.empty[Int]
    a.toList shouldBe List.empty[Int]
  }

  it("should convert a stream to the equivalent list") {
    val aStream: FpStream[String] = FpStream("A","B","C")
    aStream.toList shouldBe List("A", "B", "C")
  }

  it("Ex 5.2 should take the first n elements") {
    FpStream.empty[Int].take(4) shouldBe FpStream.empty[Int]
    FpStream(1,2,3,4).take(0) shouldBe FpStream.empty[Int]
    FpStream(1,2,3,4).take(2).toList shouldBe FpStream(1,2).toList
    FpStream(1,2,3,4).take(10).toList shouldBe FpStream(1,2,3,4).toList
  }

  it("Ex 5.3 should take elements while the condition is true") {
    val stream = FpStream("A", "B", "C")

    FpStream.empty[String].takeWhile(x => true) shouldBe FpStream.empty[String]
    stream.takeWhile(x => true).toList shouldBe stream.toList
    stream.takeWhile(x => (x == "A" || x == "B")).toList shouldBe List("A" , "B")

  }

  it("should be able to take from an infinite stream") {
    infiniteIntegers(4).take(5).toList shouldBe List(4,5,6,7,8)
  }

}
