package bbc

import org.scalatest.{FunSpec, Matchers}

class Exercise_10_5_to_10_6 extends FunSpec with Matchers {

  def foldMap[A,B](as: List[A], m : Monoid[B])(f: A => B): B = {
    as.foldRight(m.zero)((a,b) => m.op(f(a),b))
  }

  def foldLeftFromFoldMap[A,B](list: List[A], accumulator: B)(f: (B, A) => B): B = {
    val foldMonoid = new Monoid[B => B] {
      override def op(fb1: B => B, fb2: B => B) = fb2 compose fb1
      override def zero = b => b
    }
    foldMap(list, foldMonoid)(a => (b: B) => f(b,a))(accumulator)
  }

  def foldRightFromFoldMap[A,B](list: List[A], accumulator: B)(f: (A, B) => B): B = {
    val foldMonoid = new Monoid[B => B] {
      override def op(fb1: B => B, fb2: B => B) = fb1 compose fb2
      override def zero = b => b
    }
    foldMap(list, foldMonoid)(a => (b: B) => f(a, b))(accumulator)
  }

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
      foldLeftFromFoldMap(Nil: List[Int], 99)(_ + _) shouldBe 99
    }

    it("should apply the function successively from the left") {
      val result = foldLeftFromFoldMap(List("P", "Q", "R"), "")((str, acc) => str + acc)
      result shouldBe "PQR"
    }

    it("should reverse the list") {
      val result = foldLeftFromFoldMap(List(1, 2, 3), Nil: List[Int])((acc, value) => value :: acc)

      result shouldBe (List(3, 2, 1))
    }
  }

  describe("foldRight from foldMap") {
    it("should return base value for empty list") {
      foldRightFromFoldMap(Nil: List[Int], 99)(_ + _) shouldBe 99
    }

    it("should apply the function successively from the right") {
      val result = foldRightFromFoldMap(List("A", "B", "C"), "")((acc, str) => str + acc)
      result shouldBe "CBA"
    }

    it("should preserve the order of the list") {
      val result = foldRightFromFoldMap(List(1, 2, 3), Nil: List[Int])((value, acc) => value :: acc)

      result shouldBe (List(1, 2, 3))
    }
  }

}
