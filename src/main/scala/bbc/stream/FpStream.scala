package bbc.stream
import FpStream.cons

sealed trait FpStream[+A] {
  def takeWhile(fn: A => Boolean): FpStream[A] = this match {
    case Cons(hd, tl) if (fn(hd())) => cons[A](hd(),tl().takeWhile(fn))
    case _ => FpStream.empty[A]

  }

  def take(i: Int): FpStream[A] = this match {
    case Empty => FpStream.empty[A]
    case Cons(hd, tl) => if (i <= 0) Empty else cons[A](hd(), tl().take(i - 1))
  }

  def toList: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(hd, tl) => hd() :: tl().toList
  }
}

case object Empty extends FpStream[Nothing]
case class Cons[+A](h: () => A, t: () => FpStream[A]) extends FpStream[A]

object FpStream {
  def cons[A](hd: => A, tl: => FpStream[A]): FpStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: FpStream[A] = Empty

  def apply[A](as: A*): FpStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
