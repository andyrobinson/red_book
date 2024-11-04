package bbc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Excercise4_4 extends AnyFunSpec with Matchers {
  def sequence[A](a: List[Option[A]]): Option[List[A]] =  a match {
    case Nil => Some(List.empty[A])
    case head :: tail => head.flatMap(el => sequence(tail).map(el :: _))
  }

//  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
//    val empty: Option[List[A]] = Some(List.empty[A])
//    a.foldRight(empty)((optElement,acc) => optElement.flatMap(el => acc.map(list => el :: list)))
//  }

  describe("lifting Option for a sequence") {

    it("should lift the empty list to an Option of empty list") {
      sequence(List.empty[Option[Int]]) shouldBe Some(List.empty[Int])
    }

    it("should lift a sequence containing a None to None") {
      sequence(List(Some(1),Some(2),None)) shouldBe None
    }

    it("should lift a sequence only containing Somes into a option of the same list"){
      sequence(List(Some(1),Some(2),Some(3))) shouldBe Some(List(1,2,3))
    }
  }

}
