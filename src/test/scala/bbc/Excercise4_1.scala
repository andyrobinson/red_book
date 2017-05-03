package bbc

import org.scalatest.{FunSpec, Matchers}

class Excercise4_1 extends FunSpec with Matchers {
  describe("basic Option stuff") {
    it("should do map") {
      FpSome(1).map(_ + 1) shouldBe FpSome(2)
      FpNone.map(x => "blah") shouldBe FpNone
    }

    it("should do flatMap") {
      FpSome(1).flatMap(x => FpSome(x.toString)) shouldBe FpSome("1")
      FpNone.flatMap(x => FpSome("blah")) shouldBe FpNone
    }

    it("should get or else use default") {
      FpSome("a").getOrElse("b") shouldBe "a"
      FpNone.getOrElse("b") shouldBe "b"
    }

    it("should return the alternative if it's none") {
      FpSome("a").orElse(FpSome("b")) shouldBe FpSome("a")
      FpNone.orElse(FpSome("b")) shouldBe FpSome("b")
    }

    it("should only allow values which match the condition") {
      FpSome(23).filter(_ > 10) shouldBe FpSome(23)
      FpSome(9).filter(_ > 10) shouldBe FpNone
    }
  }
}
