package bbc

import org.scalatest.{FunSpec, Matchers}

object FoldableTree extends Foldable[Tree] {
  override def foldRight[A, B](collection: Tree[A])(zero: B)(fn: (A, B) => B): B =
    collection match {
      case Leaf(a) => fn(a,zero)
      case Branch(l,r) => foldRight(l)(foldRight(r)(zero)(fn))(fn)
    }

  override def foldLeft[A, B](collection: Tree[A])(zero: B)(fn: (B, A) => B): B =
    collection match {
      case Leaf(a) => fn(zero, a)
      case Branch(l,r) => foldLeft(r)(foldLeft(l)(zero)(fn))(fn)
    }

  override def foldMap[A, B](collection: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
    Tree.fold(collection)(f)(mb.op)
  }

}

class Exercise_10_13 extends FunSpec with Matchers {

  describe("tree foldable") {

    val addMonoid = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override def zero: Int = 0
    }

    val concatMonoid = new Monoid[String] {
      override def op(a1: String, a2: String): String = a1 + a2
      override def zero: String = ""
    }

    it("should do some tree stuff") {
      val tree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
      val stringTree = Branch(Leaf("A"), Branch(Branch(Leaf("B"), Leaf("C")), Leaf("D")))

      FoldableTree.foldRight(tree)(0)((x: Int,y: Int) => x + y) shouldBe 10
      FoldableTree.foldRight(stringTree)("")((x: String,y: String) => x + y) shouldBe "ABCD"

      FoldableTree.foldLeft(tree)(0)((x: Int,y: Int) => x + y) shouldBe 10
      FoldableTree.foldLeft(stringTree)("")((x: String,y: String) => x + y) shouldBe "ABCD"

      FoldableTree.foldMap(tree)((x: Int) => x + 1)(addMonoid) shouldBe 14
      FoldableTree.foldMap(stringTree)((x) => x + "@")(concatMonoid) shouldBe "A@B@C@D@"

    }
  }
}
