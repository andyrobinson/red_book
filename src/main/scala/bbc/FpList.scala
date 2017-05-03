package bbc

import scala.annotation.tailrec

sealed trait FpList[+A] {
  def tail: FpList[A]
}

case object FpNil extends FpList[Nothing] {
  override def tail: FpList[Nothing] = FpNil
}

case class Cons[+A] (head: A, tail: FpList[A]) extends FpList[A]

case class ListAndAccumulator[A,B](list:FpList[A], accumulator: B)

object FpList {
  def apply[A](as: A*): FpList[A] =
    if(as.isEmpty) FpNil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](list: FpList[A], nilValue:B)(f: (A, B) => B): B =
    list match {
      case FpNil => nilValue
      case Cons(head, tail) => f(head, foldRight(tail, nilValue)(f))
    }

  def foldLeft[A,B](list: FpList[A], accumulator: B)(f: (B, A) => B): B =
    list match {
      case FpNil => accumulator
      case Cons(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

  def map[A,B](list: FpList[A])(f:A => B): FpList[B] =
    list match {
      case FpNil => FpNil
      case Cons(head,tail) => Cons(f(head), map(tail)(f))
    }

  def filter[A](list: FpList[A])(f: (A) => Boolean): FpList[A] =
    list match {
      case FpNil => FpNil
      case Cons(head, tail) => {
        val filteredTail = filter(tail)(f)
        if (f(head)) Cons(head, filteredTail) else filteredTail
      }
    }

  def reverse[A](list: FpList[A]): FpList[A] =
    foldLeft(list, FpNil: FpList[A])((acc, value) => Cons(value, acc))

  def filterByFlatMap[A](list: FpList[A])(f: (A) => Boolean): FpList[A] =
    flatMap(list)(a => if (f(a)) FpList(a) else FpNil)

  def flatMap[A,B] (list: FpList[A]) (f: A => FpList[B]): FpList[B] = {
    foldRight(list, FpNil: FpList[B])((value, acc) => append(f(value), acc))
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
    foldRight(listOfLists, FpNil: FpList[A])((acc, list) =>
      list match {
        case FpNil => acc
        case _ =>  foldRight(acc, list)((a, b) => Cons(a, b))
      })

  def sum(list: FpList[Int]): Int =
    foldLeft(list,0)(_ + _)

  def product(list: FpList[Int]) = foldLeft(list,1)(_ * _)

  def zipSum(list1: FpList[Int], list2: FpList[Int]): FpList[Int] =
    zipWith(list1, list2)(_ + _ )

  def zipWith[A,B,C](list1: FpList[A], list2: FpList[B])(f: (A,B) => C): FpList[C] =
    (list1, list2) match {
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), zipWith(tail1, tail2)(f))
      case _ => FpNil
    }

  def addOne(list: FpList[Int]): FpList[Int] =
    map(list)(_ + 1)

  def doubleToString(list: FpList[Double]): FpList[String] =
    map(list)(_.toString)

  def hasSubsequence[A](container: FpList[A], sub: FpList[A]): Boolean = {

    def startsWith[A](container: FpList[A], starting: FpList[A]): Boolean =
      (container, starting) match {
        case (_, FpNil) => true
        case (FpNil, _) => false
        case (Cons(head, tail), Cons(starthead, starttail)) =>
          head == starthead && startsWith(tail, starttail)
      }

    container match {
      case FpNil => (sub == FpNil)
      case Cons(head, tail) => startsWith(container,sub) || hasSubsequence(tail, sub)
    }
  }


}


