package bbc

import org.scalatest.{FunSpec, Matchers}

class Exercise_10_7_to_10_9 extends FunSpec with Matchers {

   def foldMapV[A,B](as: IndexedSeq[A], m : Monoid[B])(f: A => B): B = {
     as.length match {
       case 0 => m.zero
       case 1 => f(as(0))
       case _ => {
         val (lseq,rseq) = as.splitAt(as.length/2)
         m.op(foldMapV(lseq,m)(f),foldMapV(rseq,m)(f))
       }
     }
   }

  // need min and max value because the sequences are split in half - it's not enough
  // just to keep the last value and compare it to the next - interesting
  def isOrderedSeq[A](as: IndexedSeq[A], gt: (A, A) => Boolean, minValue: A, maxValue: A): Boolean = {

    def min(a: A, b: A): A = if (gt(a,b)) b else a
    def max(a: A, b: A): A = if (gt(a,b)) a else b

    val minMaxOrderedMonoid = new Monoid[(A, A, Boolean)] {
      override def op(a1: (A, A, Boolean), a2: (A, A, Boolean)): (A, A, Boolean) =
        (min(a1._1,a2._1), max(a1._2, a2._2), a1._3 && a2._3 && gt(a2._1, a1._2))

      override def zero: (A, A, Boolean) = (minValue, maxValue, true)
    }

    foldMapV(as, minMaxOrderedMonoid)(a => (a, a, true))._3
  }

  def isOrderedIntSeq(as: IndexedSeq[Int]) = isOrderedSeq(as, (a: Int, b: Int) => a > b, Int.MinValue, Int.MaxValue)

  describe("FoldMap for IndexedSeq") {
    val addMonoid = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override def zero: Int = 0
    }

    val stringMonoid = new Monoid[String] {
      override def op(a1: String, a2: String): String = a1 + a2
      override def zero: String = ""
    }

    it("should allow us to do a fold on tuples of integers") {
      val pairs = IndexedSeq((1,2),(3,4),(5,6),(7,8),(9,10))

      /* Multiply tuple contents, then add them all */
      foldMapV(pairs, addMonoid)(x => x._1 * x._2) shouldBe 2 + 12 + 30 + 56 + 90
    }

    it("should allow us to do a fold of integers to strings") {
      foldMapV(IndexedSeq(5,6,7,8,9,10,11,12), stringMonoid)(_.toString) shouldBe "56789101112"
    }

  }

  describe("skipping the parallel exercise 10_8"){}

  describe("Testing for ordering") {

    it("should report a sequence of one or zero as ordered") {
      isOrderedIntSeq(IndexedSeq.empty[Int]) shouldBe true
      isOrderedIntSeq(IndexedSeq(99)) shouldBe true
    }

    it("should correctly report ordering of sequences") {
      isOrderedIntSeq(IndexedSeq(1,2,3,4)) shouldBe true
      isOrderedIntSeq(IndexedSeq(1,2,3,1)) shouldBe false
      isOrderedIntSeq(IndexedSeq(1,5,3,4)) shouldBe false
      isOrderedIntSeq(IndexedSeq(1,2,3,1,2,3)) shouldBe false
      isOrderedIntSeq(IndexedSeq(1,2,3,0,1,2)) shouldBe false
      isOrderedIntSeq(IndexedSeq(4,5,6,7,8,9)) shouldBe true
      isOrderedIntSeq(IndexedSeq(1,2,3,2,4,5)) shouldBe false

    }
  }
}
