package bbc

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

class Exercise2_1 extends FunSpec with Matchers {

  def fib(n: Int): Int = {

    @tailrec
    def fib3(n: Int, fibnplus1: Int, fibn: Int): Int = {
      if (n == 1) fibn
      else {
        fib3(n - 1, fibnplus1 + fibn, fibnplus1)
      }

    }

    def fib2(n: Int): (Int,Int) = {
      if (n == 2) {
        (1,0)
      }
      else {
        val (fibn,fibnnminus1) = fib2(n-1)
        (fibn + fibnnminus1,fibn)
      }
    }

    fib3(n,1,0)

//    if (n == 1) 0
//    else {
//      val (fibn,fibnminus1) = fib2(n)
//      fibn
//    }
  }

  describe("Fibonacci") {
    it("should return 0 as the first one") {
      fib(1) shouldBe 0
    }

    it ("should return 1 as the second element") {
      fib(2) shouldBe 1
    }

    it ("should sum the previous two elements for other values ") {
      fib(3) shouldBe 1
      fib(4) shouldBe 2
      fib(5) shouldBe 3
      fib(6) shouldBe 5
      fib(7) shouldBe 8
    }
  }

}
