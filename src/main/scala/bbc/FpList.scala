package bbc

import scala.annotation.tailrec

sealed trait FpList[+A] {
  def tail: FpList[A]
}

case object Nil extends FpList[Nothing] {
  override def tail: FpList[Nothing] = Nil
}

case class Cons[+A] (head: A, tail: FpList[A]) extends FpList[A]

case class ListAndAccumulator[A,B](list:FpList[A], accumulator: B)

object FpList {
  def apply[A](as: A*): FpList[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](list: FpList[A], nilValue:B)(f: (A, B) => B): B =
    list match {
      case Nil => nilValue
      case Cons(head, tail) => f(head, foldRight(tail, nilValue)(f))
    }

  def foldLeft[A,B](list: FpList[A], accumulator: B)(f: (B, A) => B): B =
    list match {
      case Nil => accumulator
      case Cons(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

  def map[A,B](list: FpList[A])(f:A => B): FpList[B] =
    list match {
      case Nil => Nil
      case Cons(head,tail) => Cons(f(head), map(tail)(f))
    }

  def filter[A](list: FpList[A])(f: (A) => Boolean): FpList[A] =
    list match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => Cons(head, filter(tail)(f))
      case Cons(_, tail) => filter(tail)(f)
    }

  def foldLeftFromRight[A,B](list: FpList[A], accumulator: B)(f: (B, A) => B): B = {
    def applyFunctionOnReturn(unusedValue: A, listAndAcc: ListAndAccumulator[A,B]): ListAndAccumulator[A,B] =
      listAndAcc.list match {
        case Cons(head, tail) => ListAndAccumulator(tail, f(listAndAcc.accumulator, head))
        case _ => listAndAcc
      }

    foldRight(list, ListAndAccumulator(list, accumulator))(applyFunctionOnReturn).accumulator
  }

//  def foldRightFromLeft[A,B](list: FpList[A], nilValue:B)(f: (A, B) => B): B = {
//    val reverseList = foldLeft(list, Nil:FpList[A])((acc,value) => Cons(value, acc))
//    foldLeft(reverseList,nilValue)((b, a) => f(a, b))
//  }

  // Official solution - really hard to understand higher order function
  def foldRightFromLeft[A,B](list: FpList[A], nilValue: B)(f: (A,B) => B): B = {
    def identity = (b:B) => b
    def delayedExecution = (doit: (B) => B, a:A) => (b: B) => doit(f(a,b))

    foldLeft(list, identity)(delayedExecution)(nilValue)
  }

  def append[A](first: FpList[A], second: FpList[A]): FpList[A] =
    foldRight(first, second)((a, b) => Cons(a, b))

  def flatten[A](listOfLists: FpList[FpList[A]]): FpList[A] =
    foldRight(listOfLists, Nil: FpList[A])((acc, list) =>
      list match {
        case Nil => acc
        case _ =>  foldRight(acc, list)((a, b) => Cons(a, b))
      })

  def sum(list: FpList[Int]): Int =
    foldLeft(list,0)(_ + _)

  def product(list: FpList[Int]) = foldLeft(list,1)(_ * _)

  def addOne(list: FpList[Int]): FpList[Int] =
    map(list)(_ + 1)

  def doubleToString(list: FpList[Double]): FpList[String] =
    map(list)(_.toString)

}


