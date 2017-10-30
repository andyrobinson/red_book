package bbc

import bbc.RNG._
import org.scalatest.{FunSpec, Matchers}

class Exercise_6_5_to_6_9 extends FunSpec with Matchers {

  describe("combinators") {
    describe("should implement double using map 6.5") {
      it("should convert integer random to double") {
        doubleMap(NotRandom(1000))._1 shouldBe (1001).toDouble/Int.MaxValue
      }
    }

    describe("should map between two different Rands 6.6") {
      it("should combine Rands") {
        val one = unit(1)
        val a = unit("A")
        val randomGenerator = SimpleRNG(6758576)

        val resultTuple = map2(a,one)((str: String, number: Int) => str + number.toString)(randomGenerator)

        resultTuple._1 shouldBe "A1"
        resultTuple._2 shouldBe randomGenerator
      }

      it("should thread the random number generator through the calls") {
        def random: Rand[Int] = _.nextInt
        val randomGenerator = SimpleRNG(1)

        val randomPair = map2(random,random)((a,b) => (a,b))(randomGenerator)

        randomPair._1 shouldBe (384748,-1151252339)
        randomPair._2 shouldNot be (randomGenerator)
      }
    }

    describe("sequence, 6.7") {
      it("should preserve an empty sequece") {
        val randomGenerator = SimpleRNG(1)
        val empty = sequence[String](List.empty[Rand[String]])(randomGenerator)
        empty shouldBe (List.empty[String],randomGenerator)
      }

      it("should pass through the RNG function for a sequence of one"){
        val one = unit(1)
        val nr = NotRandom(1000)
        val result = sequence(List(one))(nr)
        result shouldBe (List(1),nr)
      }

      it("should do longer sequences") {
        val one = unit(1)
        val two = unit(2)
        val nr = NotRandom(1000)
        val result = sequence(List(one, two, one))(nr)

        result shouldBe (List(1,2,1),nr)
      }

      it("should implement ints using sequence") {
        intsSeq(5)(NotRandom(22))._1 shouldBe List(22,22,22,22,22)
        intsSeq(10)(SimpleRNG(456))._1.length shouldBe 10
        intsSeq(6)(SimpleRNG(99))._1 shouldBe List(38090141, 1575376209, 1102671736, -565736979, -407345342, 429642468)

      }
    }

    describe("flatMap, 6.8") {
      it("should flapMap from one unit to another") {
        val one = unit(1)
        val nr = NotRandom(1000)
        flatMap(one)(x => unit(2))(nr)._1 shouldBe 2
      }

      it("should pass the state when using FlatMap") {
        def random: Rand[Int] = _.nextInt
        val rng = SimpleRNG(222)
        val firstExpected = random(rng)._1
        val secondExpected = random(random(rng)._2)._1

        val firstRand = random
        val secondRand = flatMap(firstRand)(x => random)

        firstRand(rng)._1 shouldBe firstExpected
        secondRand(rng)._1 shouldBe secondExpected
      }

      it("should produce a non-negative number less than the given one") {
        val nr100: RNG = NotRandom(100)
        val nrminus100: RNG = NotRandom(-100)
        val rng: RNG = SimpleRNG(576785)

        nonNegativeLessThan(101)(nr100)._1 shouldBe 100
        nonNegativeLessThan(99)(nr100)._1 shouldBe 1
        nonNegativeLessThan(99)(nrminus100)._1 shouldBe 1

        val random: Int = nonNegativeLessThan(500)(rng)._1
        random should be <= 500
        random should be >= 0
      }
    }
  }
}
