package bbc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Exercise2_3_to_2_5 extends AnyFunSpec with Matchers {

  def add(a:Int, b:Int) = a + b
  def stringToInt(a: String): Int = a.toInt

  def curry[A,B,C] (f: (A,B) => C): A => (B => C) =
    (a) => (b) => f(a,b)

  describe("a curry function") {

    it("should curry my two arg function") {

      val addAConstant = curry(add)
      val addThree = addAConstant(3)

      addThree(4) shouldBe 7
      addThree(66) shouldBe 69
    }
  }

  def uncurry[A,B,C] (f: A => (B => C)): (A, B) => C =
    (a, b) => f(a)(b)

  describe("an uncurry function") {
    it("should uncurry my curried function") {
      val addAConstant = curry(add)
      val addTwoNumbers = uncurry(addAConstant)

      addTwoNumbers(2,4) shouldBe 6

    }
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a) => f(g(a))

  describe("a functional composition") {

    it("should compose functions") {
      val addFive = curry(add)(5)

      val stringAddFive = compose(addFive, stringToInt)

      stringAddFive("88") shouldBe 93
    }

  }

}
