package bbc

trait Foldable[F[_]] {
  def foldRight[A,B](collection: F[A])(zero:B)(fn: (A,B) => B):B
  def foldLeft[A,B](collection: F[A])(zero:B)(fn: (B,A) => B):B
  def foldMap[A,B](collection: F[A])(f: A => B)(mb: Monoid[B]):B
  def concatenate[A](collection: F[A])(ma: Monoid[A]):A =
    foldLeft(collection)(ma.zero)(ma.op)
}
