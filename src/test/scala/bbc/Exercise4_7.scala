package bbc

import org.scalatest.{FunSpec, Matchers}
import FpEither._

class Exercise4_7 extends FunSpec with Matchers {

  it("should lift a sequence with a Left into a Left") {
    val leftSeq: List[FpEither[String, Int]] = List(Right(1), Left("nothing"), Right(3))

    sequence(leftSeq) shouldBe Left("nothing")
  }

  it("should lift a sequence with all rights Left into a Right list") {
    val leftSeq: List[FpEither[String, Int]] = List(Right(1), Right(3), Right(4))

    sequence(leftSeq) shouldBe Right(List(1,3,4))
  }

}
