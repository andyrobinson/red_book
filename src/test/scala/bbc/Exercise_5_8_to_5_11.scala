package bbc

import bbc.stream.FpStream._
import org.scalatest.{FunSpec, Matchers}

class Exercise_5_8_to_5_11 extends FunSpec with Matchers {
  describe("5.8 ") {
    it("should produce an infinite stream of any number") {
      constant(5).take(3).toList shouldBe List(5,5,5)
      constant("b").take(10).toList shouldBe List("b","b","b","b","b","b","b","b","b","b")
    }
  }
  describe("5.9") {
    it("should produce an infinite stream of increasing integers") {
      from(5).take(3).toList shouldBe List(5,6,7)
      from(-1).take(10).toList shouldBe List(-1,0,1,2,3,4,5,6,7,8)
    }
  }

  describe("5.10") {
    it("should produce the first fib") {
      fibs.take(1).toList shouldBe List(0)
    }

    it("should produce the first and second fib") {
      fibs.take(2).toList shouldBe List(0,1)
    }

    it("should produce a bunch of fibs") {
      fibs.take(10).toList shouldBe List(0,1,1,2,3,5,8,13,21,34)
    }
  }

  describe("5.11") {
    it("should finish the stream if the function returns None") {
      unfold("")(x => None).toList shouldBe List.empty[String]
    }

    it("should generate a stream of integers if the function keeps producing") {
      unfold(2)(x => Some((x,x))).take(5).toList shouldBe List(2,2,2,2,2)
    }

    it("can use a generator which increases") {
      unfold("A")(x => Some((x, x.substring(0,1) + x))).take(6).toList shouldBe List("A","AA","AAA","AAAA","AAAAA","AAAAAA")
    }

    it("can generate fib") {
      val fib10 = unfold(0,1)({case (n,n1) => Some((n,(n1, n + n1)))}).take(10).toList
      fib10 shouldBe List(0,1,1,2,3,5,8,13,21,34)
    }
  }
}
