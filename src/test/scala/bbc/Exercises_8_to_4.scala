package bbc

import bbc.Prop.{FailedCase, GenSimple}
import org.scalatest.{FunSpec, Matchers}

trait PropBoolean {
  def check: Boolean
  def && (p: PropBoolean): PropBoolean = {
    val thisCheck = check
    new PropBoolean {
      override def check = {
        thisCheck && p.check
      }
    }
  }
}

object Prop {
  type FailedCase = String
  // type SuccessCount = Int // causes lots of compiler problems
  type GenSimple[A] = State[RNG,A]
}

trait Prop { def check: Either[FailedCase,Int] }

object GenSimple {
  def choose(start: Int, stopExclusive: Int): GenSimple[Int] = {
    val state = State(RNG.nonNegativeInt)
    state.map(n => start + n % (stopExclusive - start))
  }

  // This skips over values until they are in the right range
  // The solution suggested is to generate a non-negative number in the
  // correct range and add it to the base value - of course
  def chooseAttemptPoor(start: Int, stopExclusive: Int): GenSimple[Int] =
    State(rng => {
      RNG.until(rng)(x => x >= start && x < stopExclusive)
    })
}

case class seqRNG(seq: Seq[Int]) extends RNG {
  override def nextInt: (Int, RNG) = seq match {
    case hd :: tl => (hd, seqRNG(tl))
    case _ => (0, seqRNG(Nil))
  }
}

class Exercises_8 extends FunSpec with Matchers {
  describe("Prop with boolean check") {

    val success = new PropBoolean {
      override def check: Boolean = true
    }

    val failure = new PropBoolean {
      override def check: Boolean = false
    }

    it("should propagate success") {
      (success && success).check shouldBe true
    }

    it("should propagate failure") {
      (failure && success).check shouldBe false
    }

  }

  describe("Generate a range of integers") {
    it("should do something") {
      val sequence = seqRNG(Seq(5,7,3,6,4))

      val gen = GenSimple.choose(2,6)
      val result1 = gen.run(sequence)
      val result2 = gen.run(result1._2)
      val result3 = gen.run(result2._2)

      result1._1 shouldBe 2 + (5 % 4)
      result2._1 shouldBe 2 + (7 % 4)
      result3._1 shouldBe 2 + (3 % 4)
    }
  }

}
