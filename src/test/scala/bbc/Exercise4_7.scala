package bbc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import FpEither._

class Exercise4_7 extends AnyFunSpec with Matchers {

  def moreThan2(i: Int): FpEither[String, Int] = if (i > 2) Right(i) else Left(s"${i} is too small")

  it("should lift a sequence with a Left into a Left") {
    val leftSeq: List[FpEither[String, Int]] = List(Right(1), Left("nothing"), Right(3))

    sequence(leftSeq) shouldBe Left("nothing")
  }

  it("should lift a sequence with all rights Left into a Right list") {
    val leftSeq: List[FpEither[String, Int]] = List(Right(1), Right(3), Right(4))

    sequence(leftSeq) shouldBe Right(List(1,3,4))
  }

  it("should apply a function across the list then lift the result to a left if any of the results are left") {
    val seqWithSmallValue = List(3,4,1,5)
    traverse(seqWithSmallValue)(moreThan2) shouldBe Left("1 is too small")
  }

  it("should apply a function across the list then lift the result to a right if all applications return right") {
    val seq = List(3,4,99,5)
    traverse(seq)(moreThan2) shouldBe Right(List(3,4,99,5))
  }

}
