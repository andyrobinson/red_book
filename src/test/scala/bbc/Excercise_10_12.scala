package bbc

import org.scalatest.{FunSpec, Matchers}


object FoldableFpList extends Foldable[FpList] {

  override def foldRight[A, B](collection: FpList[A])(zero: B)(fn: (A, B) => B): B =
    FpList.foldRight(collection,zero)(fn)

  override def foldLeft[A, B](collection: FpList[A])(zero: B)(fn: (B, A) => B): B =
    FpList.foldLeft(collection, zero)(fn)

  override def foldMap[A, B](collection: FpList[A])(fn: A => B)(mb: Monoid[B]): B =
    FpList.foldRight(FpList.map(collection)(fn), mb.zero)(mb.op)
}

class Excercise_10_12 extends FunSpec with Matchers {

  describe("Foldable List") {

    val addMonoid = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override def zero: Int = 0
    }

    it("should implement the functions correctly") {
      var x = FpList(1,2,3,4)
      FoldableFpList.foldRight(x)(0: Int)((x: Int,y: Int) => x + y) shouldBe 10
      FoldableFpList.foldLeft(x)(1: Int)((x: Int,y: Int) => x * y) shouldBe 24
      FoldableFpList.foldMap(x)(x => x + 1)(addMonoid) shouldBe 14
    }

    // other implementations are more or less identical  - not really interesting

  }

}
