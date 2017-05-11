package bbc

import org.scalatest.{FunSpec, Matchers}

import math._
import scala.util.Try

class Exercise4_2 extends FunSpec with Matchers {

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum/xs.size)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(mv => mean(xs.map(x => math.pow(x - mv,2))))
  }

  it("should return None if the sequence is empty") {
    variance(Seq.empty[Double]) shouldBe None
  }

  it("should return zero if there is only one element in the sequence") {
    variance(Seq(54.0)) shouldBe Some(0.0)
  }

  it("should return the mean of the square of the difference between the values and the mean for sequences") {
    variance(Seq(1.0,5.0)) shouldBe Some(4.0)
    variance(Seq(2.0,4.0)) shouldBe Some(1.0)
  }

}
