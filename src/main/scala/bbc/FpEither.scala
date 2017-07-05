package bbc

sealed trait FpEither[+E, +A] {

  //  def map[B](f: A => B): FpEither[E, B] = this match {
  //    case Right(x) => Right(f(x))
  //    case Left(x) => Left(x)
  //  }

  def flatMap[EE >:E, B](f: A => FpEither[EE,B]): FpEither[EE, B] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }

  def map[B](f: A => B): FpEither[E, B] = flatMap(x => Right(f(x)))

  def orElse[EE >: E, B >: A](b: => FpEither[EE, B]): FpEither[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(x) => b
  }

  def map2[EE >: E,B,C](b: FpEither[EE,B])(f: (A,B) => C): FpEither[EE,C] =
    this.flatMap(aa => b.map(bb => f(aa,bb)))
  //    for {
  //      a <- this
  //      bb <- b
  //    } yield f(a,bb)

}

case class Left[+E](value: E) extends FpEither[E, Nothing]
case class Right[+A](value: A) extends FpEither[Nothing, A]

object FpEither {

  def sequence[E, A](es: List[FpEither[E, A]]): FpEither[E, List[A]] =
    es match {
      case Nil  => Right(List.empty[A])
      case headEither :: tailEither => for {
        head <- headEither
        tail <- sequence(tailEither)
      } yield head :: tail
    }

  def traverse[E, A, B](as: List[A]) (f: A => FpEither[E, B]): FpEither[E, List[B]] = as match {
    case Nil => Right(List.empty[B])
    case head :: tail => for {
      hd <- f(head)
      tl <- traverse(tail)(f)
    } yield hd :: tl
  }

}

