package bbc

sealed trait FpOption[+A] {
  def map[B](f: A => B):FpOption[B] = this.flatMap(x => FpSome(f(x)))

  def flatMap[B](f: A => FpOption[B]): FpOption[B] = this match {
    case FpSome(x) => f(x)
    case _ => FpNone
  }

  def flatMap2[B](f: A => FpOption[B]): FpOption[B] =
    this.map(f).getOrElse(FpNone)

  def getOrElse[B >: A](default: => B): B = this match {
    case FpSome(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => FpOption[B]): FpOption[B] =
    this.map(FpSome(_)).getOrElse(ob)

  def filter(f: A => Boolean): FpOption[A] =
    this.flatMap(x =>  if (f(x)) this else FpNone)
}

case class FpSome[+A](get: A) extends FpOption[A]
case object FpNone extends FpOption[Nothing]

