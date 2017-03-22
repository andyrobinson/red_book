package bbc

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

class Exercise_3_1_to_3_6 extends FunSpec with Matchers {
  describe("Exercise 3.1 pattern matching") {
    it("pick out the right one") {
      val x: Int = FpList(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h
        case _ => 101
      }
      x shouldBe 3
    }

  }

  describe("Exercise 3.2 tail") {

    def tail[A](list: FpList[A]): FpList[A] =
      list match {
        case Cons(_, t) => t
        case _ => Nil
      }

    it("should get the tail of the list") {
      val x = FpList(1,2,3)

      x.tail shouldBe FpList(2,3)
      Nil.tail shouldBe Nil // could also throw an error, use an option
    }

    it("should get the tail of the list using the function") {
      val x = FpList(1,2,3)

      tail(x) shouldBe FpList(2,3)
      tail(Nil) shouldBe Nil
    }

  }

  describe("Exercise 3.3 setHead") {

    def setHead[A](newHead: A, list: FpList[A]): FpList[A] =
      list match {
        case Cons(oldHead, tail) => Cons(newHead, tail)
        case _ => Nil
      }

    it("should return Nil if the list is Nil ") {
      setHead[Int](1,Nil) shouldBe Nil
    }

    it("should replace the head of the list") {
      val x = FpList("A","B","CCC")

      setHead("DDD",x) shouldBe FpList("DDD","B","CCC")
    }
   }

  describe("Exercise 3.4 drop") {

    @tailrec
    def drop[A](list: FpList[A], n: Int): FpList[A] =
      if (n == 0) list
      else
        list match {
          case Cons(_, t) => drop(t,n-1)
          case _ => Nil
        }

    it("should get the list if n is zero") {
      val x = FpList(1,2,3)
      drop(x,0) shouldBe x
    }

    it("should get the nth tail if n is more than zero") {
      val x = FpList(1,2,3)
      drop(x,2) shouldBe FpList(3)
    }

    it("should return Nil if we run out of list") {
      val x = FpList(1,2,3)
      drop(x,4) shouldBe Nil
    }


  }

  describe("Exercise 3.5 dropWhile") {

    @tailrec
    def dropWhile[A](list: FpList[A], f: A => Boolean): FpList[A] = {
      list match {
        case Cons(el, tail) if f(el) => dropWhile(tail, f)
        case _ => list
      }
    }

    it ("should return Nil for a Nil list") {
      dropWhile[Int](Nil, _ > 2) shouldBe Nil
    }

    it("should return Nil if the condition is true for all elements") {
      val x = FpList(1,2,3)

      dropWhile[Int](x, _ < 10) shouldBe Nil
    }

    it("should return a list dropping elements until the condition is false") {
      val x = FpList("A", "A", "B", "C")

      dropWhile[String](x, _ == "A") shouldBe FpList("B", "C")
    }

  }

  describe ("Exercise 3.6 init") {

    def init[A](list: FpList[A]): FpList[A] = {

      @tailrec
      def reverse2[A](l: FpList[A], acc: FpList[A]): FpList[A] =
        l match {
          case Nil => acc
          case Cons(h,tl) => reverse2(tl,Cons(h, acc))
        }

      @tailrec
      def init2[A](l: FpList[A], acc: FpList[A]): FpList[A] =
        l match {
          case Cons(h, Cons(h2, tl)) => init2(Cons(h2, tl), Cons(h, acc))
          case _ => reverse2(acc, Nil)
        }

      init2(list, Nil)
    }

    it("should return Nil if the list is Nil") {
      init(Nil) shouldBe Nil
    }

    it("should return Nil for a single item list") {
      init(Cons("A", Nil)) shouldBe Nil
    }

    it("should return all but the last element for non empty lists") {
      val x = FpList(1,2,3,4)

      init(x) shouldBe FpList(1,2,3)
    }
  }

}
