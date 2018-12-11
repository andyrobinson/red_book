package bbc

import org.scalatest.{FunSpec, Matchers}

class Exercise10_1_to_10_3 extends FunSpec with Matchers {

  protected def SatisfiesMonoidLaws[A](monoid: Monoid[A], a1:A, a2: A, a3: A, includeZeroTestCaseTest: Boolean = true): Unit = {
    import monoid._
    if (includeZeroTestCaseTest) {
      withClue("Test values must not be zero") {a1 shouldNot be(zero)}
      withClue("Test values must not be zero") {a2 shouldNot be(zero)}
      withClue("Test values must not be zero") {a3 shouldNot be(zero)}
    }
    withClue("Associativity") {op(op(a1,a2),a3) shouldBe op(a1, op(a2,a3))}
    withClue("Zero behaviour") {op(zero,a1) shouldBe a1}
    withClue("Zero behaviour") {op(a2,zero) shouldBe a2}
  }

  describe("Monoid") {
    it("should confirm it's a Monoid of String using concatenation and empty") {
      val stringMonoid = new Monoid[String] {
        override def op(a1: String, a2: String): String = a1 + a2
        override def zero: String = ""
      }

      SatisfiesMonoidLaws(stringMonoid, "a", "b", "c")
    }

    it("should confirm it's a Monoid of Int using addition and zero") {
      val intAddMonoid = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 + a2

        override def zero: Int = 0
      }

      SatisfiesMonoidLaws(intAddMonoid, 44, 22, 36)
    }

    it("should confirm integer multiplication monoid using 1") {
      val intMultiplyMonoid = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 * a2

        override def zero: Int = 1
      }
      SatisfiesMonoidLaws(intMultiplyMonoid, 44, 0, -67)
    }

    it("should confirm boolean OR monoid using false") {
      val BooleanOrMonoid = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

        override def zero: Boolean = false
      }
      SatisfiesMonoidLaws(BooleanOrMonoid, true, false, true, includeZeroTestCaseTest = false)
    }

    it("should confirm boolean AND monoid using true") {
      val BooleanOrMonoid = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

        override def zero: Boolean = true
      }
      SatisfiesMonoidLaws(BooleanOrMonoid, true, false, true, includeZeroTestCaseTest = false)
    }

    it("should confirm option monoid") {
      def OptionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
        override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.fold(a2)(_ => a1)

        override def zero: Option[A] = None
      }
      val OptionStringMonoid = OptionMonoid[String]

      SatisfiesMonoidLaws(OptionStringMonoid, Some("A"), None, Some("B"), false)
      SatisfiesMonoidLaws(OptionStringMonoid, None, Some("B"), None, false)
    }

    it("should be a monoid for endofunctions") {
      def endoMonoid[A] = new Monoid[A => A] {
        override def op(a1: (A) => A, a2: (A) => A): (A) => A = x => a2(a1(x))

        override def zero: (A) => A = (x: A)  => x
      }

      val IntEndoMonoid = endoMonoid[Int]

      def a1 (x: Int) = x + 1
      def a2 (y: Int) = y - 2
      def a3 (z: Int) = z * 2

      import IntEndoMonoid._
      op(op(a1,a2),a3)(45) shouldBe op(a1, op(a2,a3))(45)
      op(zero,a1)(64) shouldBe a1(64)
      op(a2,zero)(-12) shouldBe a2(-12)
    }
  }
}
