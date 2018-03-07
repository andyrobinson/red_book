package bbc

import org.scalatest.{FunSpec, Matchers}

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

  val fixedRNG = fixed(1)
  val seqRandom = seqRNG(Seq(10,67,3,42,11,17,87))

  describe("Gen exercise 8.5") {


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

    it("should generate strings") {
      val seqAsciiOffsetBy48: RNG = seqRNG(Seq(24,53,60,60,63))

      val a = Gen.string(5)
      a.sample.run(seqAsciiOffsetBy48)._1 shouldBe "Hello"
    }
  }

  describe("Gen exercise 8.6") {
    it("should do flatMap") {
      val gen = Gen.unit("A")

      val genFlatMapped = gen.flatMap(x => Gen.unit(x + "B"))

      val(n,rng2) = genFlatMapped.sample.run(fixedRNG)
      n shouldBe "AB"
    }

    it("should generate a list of numbers") {
      val gen1 = Gen.unit("Z")

      val gen2 = gen1.listOfN(Gen.choose(5,10)) // first number from  choose seqRandom is 5
      gen2.sample.run(seqRandom)._1 shouldBe List("Z","Z","Z","Z","Z") // so list of length 5 is generated
    }

  }
}
