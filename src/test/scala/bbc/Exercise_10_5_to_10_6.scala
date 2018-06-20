package bbc

import org.scalatest.{FunSpec, Matchers}

class Exercise_10_5_to_10_6 extends FunSpec with Matchers {

  def foldMap[A,B](as: List[A], m : Monoid[B])(f: A => B): B = {
    as.foldRight(m.zero)((a,b) => m.op(f(a),b))
  }

  def foldLeftFromFoldMap[A,B](list: FpList[A], accumulator: B)(f: (B, A) => B): B = ???

  val addMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  describe("foldMap") {
    it("should allow us to do a fold on tuples of integers") {
      val pairs = List((1,2),(3,4),(5,6))

      /* Multiple tuple contents, then add them all */
      foldMap(pairs, addMonoid)(x => x._1 * x._2) shouldBe 44
    }

    it("should allow us to do a fold of integers to strings") {
      foldMap(List(5,6,7,8,9), stringMonoid)(_.toString) shouldBe "56789"
    }
  }

  describe("foldLeft from foldMap") {
    it("should return base value for empty list") {
      foldLeftFromFoldMap(FpNil: FpList[Int], 99)(_ + _) shouldBe 99
    }

    it("should apply the function successively from the left") {
      val result = foldLeftFromFoldMap(FpList("P", "Q", "R"), "")((str, acc) => str + acc)
      result shouldBe "PQR"
    }

    it("should reverse the list") {
      val result = foldLeftFromFoldMap(FpList(1, 2, 3), FpNil: FpList[Int])((acc, value) => Cons(value, acc))

      result shouldBe (FpList(3, 2, 1))
    }
  }
}
