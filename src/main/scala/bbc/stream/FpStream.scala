package bbc.stream
import FpStream._

sealed trait FpStream[+A] {
  def headOption: Option[A] =
    this.foldRight(Option.empty[A])((x, _) => Some(x))

  def takeWhile(fn: A => Boolean): FpStream[A] = this match {
    case Cons(hd, tl) if (fn(hd())) => cons[A](hd(),tl().takeWhile(fn))
    case _ => FpStream.empty[A]
  }

  def takeWhileFR(fn: A => Boolean): FpStream[A] =
    this.foldRight(empty[A])((x, acc) => if (fn(x)) cons[A](x, acc) else Empty)

  def take(i: Int): FpStream[A] = this match {
    case Empty => FpStream.empty[A]
    case Cons(hd, tl) => if (i <= 0) Empty else cons[A](hd(), tl().take(i - 1))
  }

  def toList: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(hd, tl) => hd() :: tl().toList
  }

  def foldRight[B](z: => B)(f :(A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
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
