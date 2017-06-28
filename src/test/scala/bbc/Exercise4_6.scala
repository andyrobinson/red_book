package bbc

import org.scalatest.{FunSpec, Matchers}

class Exercise4_6 extends FunSpec with Matchers {

  it("should implement map") {
    val right: FpEither[String, Int] = Right(1)
    val left: FpEither[String, Int] = Left("not a value")

    right.map(_ + 2) shouldBe Right(3)
    left.map(_ + 2) shouldBe left
  }

  it("should implement flatMap") {
    val right: FpEither[String, Int] = Right(1)
    val left: FpEither[String, Int] = Left("not a value")

    right.flatMap(x => Right(x + 99)) shouldBe Right(100)
    left.flatMap(x => Right(x + 99)) shouldBe left
  }

  it("should implement orElse") {
    val right: FpEither[String, Int] = Right(1)
    val left: FpEither[String, Int] = Left("not a value")
    val default: FpEither[String, Int] = Right(4)

    right.orElse(default) shouldBe right
    left.orElse(default) shouldBe Right(4)
  }

  it("should lift a function of 2 arguments to work on either") {
    val right: FpEither[String, Int] = Right(2)
    val right2: FpEither[String, Int] = Right(3)
    val left: FpEither[String, Int] = Left("not a value")
    val add2 = (a: Int, b: Int) => a + b

    right.map2(right2)(add2) shouldBe Right(5)
    right.map2(left)(add2) shouldBe left
    left.map2(right2)(add2) shouldBe left
  }

}
