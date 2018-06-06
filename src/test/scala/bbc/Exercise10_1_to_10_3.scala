package bbc

package bbc

import org.scalatest.{FunSpec, Matchers}

trait Monoid[A] {
  def op(a1: A, a2 : A): A
  def zero: A
}

class Exercise10_1_to_10_3 extends FunSpec with Matchers {

  private def SatisfiesMonoidLaws[A](monoid: Monoid[A], a1:A, a2: A, a3: A): Unit = {
    import monoid._
    op(op(a1,a2),a3) shouldBe op(a1, op(a2,a3))
    op(zero,a1) shouldBe op(a1,zero)
  }

  describe("Monoid") {
    it("should confirm it's a Monoid of String") {

      val stringMonoid = new Monoid[String] {
        override def op(a1: String, a2: String): String = a1 + a2
        override def zero: String = ""
      }

      SatisfiesMonoidLaws(stringMonoid, "a", "b", "c")
    }


  }
}
