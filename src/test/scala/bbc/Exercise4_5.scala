package bbc

import org.scalatest.{FunSpec, Matchers}

import scala.util.Try

class Exercise4_5 extends FunSpec with Matchers {

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(List.empty[B])
    case head :: tail => f(head).flatMap(hd => traverse(tail)(f).map(hd :: _))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  it("should return the empty list if the list is empty") {
    traverse(List.empty[String])(x => Try(x.toInt).toOption) shouldBe Some(List.empty[Int])
  }

  it("should return Some list of values if every value is something") {
    traverse(List("45", "23"))(x => Try(x.toInt).toOption) shouldBe Some(List(45,23))
  }

  it("should return None if any value results in None") {
    traverse(List("45", "23", "dog", "99"))(x => Try(x.toInt).toOption) shouldBe None
  }

  it("should implement sequence in terms of map") {
    sequence(List.empty[Option[Int]]) shouldBe Some(List.empty[Int])
    sequence(List(Some(1),Some(2),None)) shouldBe None
    sequence(List(Some(1),Some(2),Some(3))) shouldBe Some(List(1,2,3))
  }


}
