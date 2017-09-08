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

  // fold right implementations
  def map[B](f: A => B): FpStream[B] =
    foldRight(empty[B])((x, acc) => cons(f(x), acc))

  def filter(f: A => Boolean): FpStream[A] =
    foldRight(empty[A])((x, acc) => if(f(x)) cons(x,acc) else acc)

  def append[B >: A](s: => FpStream[B]): FpStream[B] =
    foldRight(s)((x, acc) => cons(x,acc))

  def flatMap[B](f: A => FpStream[B]): FpStream[B] =
    foldRight(empty[B])((x, acc) => f(x).append(acc))

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

  def constant[A](a: A): FpStream[A] = cons(a, constant(a))

  def from(n: Int): FpStream[Int] = cons(n, from(n+1))

  def fibs: FpStream[Int] = fibs2(0,1)

  private def fibs2(n: Int, n1: Int): FpStream[Int] =
    cons(n, fibs2(n1, n + n1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): FpStream[A] = f(z) match {
    case Some((a,s)) => cons(a,unfold(s)(f))
    case _ => Empty
  }

}
