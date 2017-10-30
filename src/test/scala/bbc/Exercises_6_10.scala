package bbc
import bbc.State._


import org.scalatest.{FunSpec, Matchers}

class Exercises_6_10  extends FunSpec with Matchers {
  def counter(currentValue: Int): (Int, Int) = (currentValue, currentValue + 1)
  def stringCounter(currentValue: Int): (String, Int) = (currentValue.toString, currentValue + 1)

  describe("unit") {
    it("should return a constant value") {
      val stateThing = unit[String,Int](1)
      stateThing.run("blah") shouldBe (1, "blah")
      stateThing.run("oops") shouldBe (1, "oops")
    }

    it("should implement a counter") {
      val counterStateThing = State(counter)
      counterStateThing.run(1) shouldBe (1,2)
      counterStateThing.run(33) shouldBe (33,34)

    }
  }

  describe("map") {
    it("should make it count in 2s") {
      val counterState: State[Int, Int] = State(counter)
      val doubleCounter = counterState.map(x => x * 2)
      doubleCounter.run(1) shouldBe(2, 2)
      doubleCounter.run(46) shouldBe(92, 47)
    }
  }

  describe("map2") {
    it("should do one count after another and then multiply the results by 5"){
      val counterState: State[Int, Int] = State(counter)
      val twoCounts = counterState.map2(counterState)((a,b) => (a + b) * 5)
      twoCounts.run(1) shouldBe (15,3)
      twoCounts.run(7) shouldBe (75,9)
    }
  }

  describe("sequence"){
    it("should do a sequence of counts, threading the state through") {
      val counterState: State[Int, Int] = State(counter)
      val countSeq = sequence(List(counterState, counterState, counterState))
      countSeq.run(1) shouldBe (List(1,2,3),4)
    }
  }

  describe("flatMap - should have done this first!") {
    it("should create new state") {
      val counterState: State[Int, Int] = State(counter)
      val stepState = counterState.flatMap(i => State(x => (x, x+i)))

      stepState.run(1) shouldBe (2,3)
      stepState.run(42) shouldBe (43,85)
    }

    it("should convince me that this is right") {
      val counterState: State[Int, Int] = State(counter)
      val stringState = counterState.flatMap(i => State(x => ("Value is " + x.toString, x)))

      stringState.run(1) shouldBe ("Value is 2",2)

    }
  }

}
