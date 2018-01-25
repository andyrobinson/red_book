package bbc.gen2

import bbc.{RNG, State}
import org.scalatest.{FunSpec, Matchers}

case class Gen[+A](sample: State[RNG,A])

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(rng => {
    val (n, rng2) = rng.nextInt
    ((n % 2 == 0), rng2)
  }))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen({
    val state = State(RNG.nonNegativeInt)
    state.map(n => start + n % (stopExclusive - start))
  })

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen({
    val stateList = List.fill(n)(State(g.sample.run))
    State.sequence(stateList)
  })
}

case class seqRNG(seq: Seq[Int]) extends RNG {
  override def nextInt: (Int, RNG) = seq match {
    case hd :: tl => (hd, seqRNG(tl))
    case _ => (0, seqRNG(Nil))
  }
}

case class fixed(int: Int) extends RNG {
  override def nextInt: (Int, RNG) = (int, fixed(int))
}

class Exercises_8_5_to10 extends FunSpec with Matchers {
  describe("Gen exercise 8.5") {

    val fixedRNG = fixed(1)
    val seqRandom = seqRNG(Seq(10,67,3,42,11,17,87))

    it("should implement unit") {
      val a = Gen.unit("A")
      val (n,rng2) = a.sample.run(fixedRNG)
      n shouldBe "A"
    }

    it("should generate booleans by sampling") {
      val a = Gen.boolean
      val (b,_) = a.sample.run(fixed(1))
      val (c,_) = a.sample.run(fixed(2))
      b shouldBe false
      c shouldBe true
    }

    it("should generate a list of numbers") {
      val a = Gen.listOfN(5, Gen.choose(5,10))
      a.sample.run(seqRandom)._1 shouldBe List(5,7,8,7,6)
    }
  }
}
