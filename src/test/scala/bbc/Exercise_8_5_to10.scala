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
  val seqRandom: seqRNG = seqRNG(Seq(10,67,3,42,11,17,87))

  def runTimes[A](g: Gen[A], r: RNG, t: Int): List[A] =
    runTimesAcc(g,r,t,List.empty[A])

  def runTimesAcc[A](g: Gen[A], r: RNG, t: Int, acc: List[A]): List[A]  =
    if (t > 0) {
      val (v,s2) = g.sample.run(r)
      runTimesAcc(g,s2,t-1,v :: acc)
    }
    else acc.reverse

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

    it("should generate a list of n values") {
      val gen1 = Gen.unit("Z")

      val gen2 = gen1.listOfGenSize(Gen.choose(5,10)) // first number from  choose seqRandom is 5
      gen2.sample.run(seqRandom)._1 shouldBe List("Z","Z","Z","Z","Z") // so list of length 5 is generated
    }

  }

  describe("Exercise 8.7") {
    it("should combine generators and pick values from each") {
      val gen1 = Gen.unit("A")
      val gen2 = Gen.unit("B")

      val unionGen = Gen.union(gen1,gen2)
      val result = runTimes(unionGen,seqRNG(Seq(0,1,0,1,1)),5)
      result shouldBe List("A","B","A","B","B")

    }
  }

  describe("Exercise 8.8") {
    it("should combine generators and pick values from each according to the weights") {
      val fiftyPercent = Int.MaxValue/2
      val thirtyPercent = Int.MaxValue/3
      val eightyPercent = (Int.MaxValue/5) * 4

      val gen1 = Gen.unit("A")
      val gen2 = Gen.unit("B")

      val weightedGen = Gen.weighted((gen1,0.2),(gen2,0.8))
      val result = runTimes(weightedGen,seqRNG(Seq(0,fiftyPercent, thirtyPercent, eightyPercent, fiftyPercent)),5)
      result shouldBe List("A","B","B","B","B")
    }
  }

}
