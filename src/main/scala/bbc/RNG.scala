package bbc

import scala.annotation.tailrec
import scala.math.abs

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  def until(rng: RNG)(pred: Int => Boolean): (Int, RNG) = {
    val (next,rng2) = rng.nextInt
    if (pred(next)) (next,rng2) else until(rng2)(pred)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (next,rng2) = rng.nextInt
    val safeNext = if (next == Int.MinValue) Int.MaxValue else abs(next)
    (safeNext,rng2)
  }

  def nonNegativeInt2: Rand[Int] =
  rng => {
    val (next,rng2) = rng.nextInt
    val safeNext = if (next == Int.MinValue) Int.MaxValue else abs(next)
    (safeNext,rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (next, rng2) = nonNegativeInt(rng)
    ((next+1).toDouble/Int.MaxValue,rng2)
  }

  def intDouble(rng: RNG): ((Int,Double),RNG) = {
    val (nextInt,rng2) = nonNegativeInt(rng)
    val (nextDouble,rng3) = double(rng2)
    ((nextInt,nextDouble),rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int),RNG) ={
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }

  def double3(rng:RNG): ((Double,Double,Double),RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def ints(repeat: Int)(rng:RNG): (List[Int], RNG) = {
    if (repeat <= 0)
      (List(),rng)
    else {
      val (next,r2) = rng.nextInt
      val (rest, r3) = ints(repeat-1)(r2)
      ((next :: rest),r3)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map_v1[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleMap: Rand[Double] =
    map(nonNegativeInt)(i => (i+1).toDouble/Int.MaxValue)

  def map2_v1[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  // non tail recursive solution
  def sequence_simple[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case  hd :: tl => rng => {
      val (a, rng2) = hd(rng)
      val (rest, rng3) = sequence_simple(tl)(rng)
      (a :: rest, rng3)
    }
    case _ => unit(List.empty[A])
  }

  // tail recursive, very hard
  def sequence_tail[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => sequence2(fs, List.empty[A], rng)

  @tailrec
  def sequence2[A](fs: List[Rand[A]], acc: List[A], nextRng: RNG): (List[A], RNG) = fs match {
    case hd :: tl => {
      val (a, rng2) = hd(nextRng)
      sequence2(tl, a :: acc, rng2)
    }
    case _ => (acc, nextRng)
  }

  // should be using foldRight (sigh)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((rnd, acc) => map2(rnd, acc) (_ :: _))

  def intsSeq(repeat: Int)(rng:RNG): (List[Int], RNG) = {
    val listOfRands: List[Rand[Int]] = List.fill(repeat)(_.nextInt)
    sequence(listOfRands)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) -mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) (a => unit(f(a)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

}
