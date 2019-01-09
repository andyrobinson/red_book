package bbc

import org.scalatest.{FunSpec, Matchers}

object FoldableOption extends Foldable[Option] {
  override def foldRight[A, B](collection: Option[A])(zero: B)(fn: (A, B) => B): B =
    collection match {
      case Some(x) => fn(x, zero)
      case _ => zero
    }

  override def foldLeft[A, B](collection: Option[A])(zero: B)(fn: (B, A) => B): B =
    collection match {
      case Some(x) => fn(zero,x)
      case _ => zero
    }

  override def foldMap[A, B](collection: Option[A])(f: A => B)(mb: Monoid[B]): B =
    collection match {
      case Some(x) => f(x)
      case _ => mb.zero
    }

}

class Exercise_10_14 extends FunSpec with Matchers {

  describe("Foldable Option") {

    val addMonoid = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override def zero: Int = 0
    }

    it("should fold none to zero value") {
      val a: Option[Int] = None

      FoldableOption.foldLeft(a)(0)((x: Int,y: Int) => x + y) shouldBe 0
      FoldableOption.foldLeft(a)(99)((x: Int,y: Int) => x + y) shouldBe 99

      FoldableOption.foldRight(a)(0)((x: Int,y: Int) => x + y) shouldBe 0
      FoldableOption.foldRight(a)(99)((x: Int,y: Int) => x + y) shouldBe 99

    }

    it("should fold some") {
      val a: Option[Int] = Some(33)

      FoldableOption.foldLeft(a)(0)((x: Int,y: Int) => x + y) shouldBe 33
      FoldableOption.foldLeft(a)(99)((x: Int,y: Int) => x + y) shouldBe 33 + 99

      FoldableOption.foldRight(a)(0)((x: Int,y: Int) => x + y) shouldBe 33
      FoldableOption.foldRight(a)(99)((x: Int,y: Int) => x + y) shouldBe 33 + 99
    }

    it("should use the monoid values") {
      FoldableOption.foldMap(Some(22))((x:Int) => x + 1)(addMonoid) shouldBe 23
      FoldableOption.foldMap(None)((x:Int) => x + 1)(addMonoid) shouldBe 0
    }
  }

}
