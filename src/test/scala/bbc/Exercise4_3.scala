package bbc

import org.scalatest.{FunSpec, Matchers}

class Exercise4_3 extends FunSpec with Matchers {

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=>C): Option[C] =
      a.flatMap(a1 => b.map(b1 => f(a1,b1)))
//    for {
//      a1 <- a
//      b1 <- b
//    } yield f(a1,b1)

  describe("should lift a function with two arguments to one which accepts options") {

    it("should return None if either argument is None") {
      map2(None, Some(1))((a: Int,b: Int) => a+b) shouldBe None
      map2(Some(99), None)((a: Int,b: Int) => a+b) shouldBe None
    }

    it("should return Some value if both arguments are not None") {
      map2(Some("A"),Some("B"))((a: String,b: String) => a+b) shouldBe Some("AB")
    }
  }
}
