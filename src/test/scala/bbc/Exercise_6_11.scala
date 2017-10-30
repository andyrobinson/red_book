package bbc

import org.scalatest.{FunSpec, Matchers}
import CandyMachine._

class Exercise_6_11 extends FunSpec with Matchers{
  describe("the Candy Machine") {
    it("a locked machine with candy should accept a coin and unlock") {
      val machine = CandyMachine(true, 2, 0)
      val ((candies, coins), newMachine) = simulateMachine(List(Coin)).run(machine)

      candies shouldBe 2
      coins shouldBe 1
      newMachine.locked shouldBe false
    }

    it("an unlocked machine with candy will dispense and lock") {
      val machine = CandyMachine(false, 2, 4)
      val ((candies, coins), newMachine) = simulateMachine(List(Turn)).run(machine)

      candies shouldBe 1
      coins shouldBe 4
      newMachine.locked shouldBe true
    }

    it("it should do nothing when turing the knob on a locked machine") {
      val machine = CandyMachine(true, 2, 4)
      val ((candies, coins), newMachine) = simulateMachine(List(Turn)).run(machine)

      candies shouldBe 2
      coins shouldBe 4
      newMachine.locked shouldBe true
    }

    it("it should do nothing when inserting a coin into an unlocked machine") {
      val machine = CandyMachine(false, 2, 4)
      val ((candies, coins), newMachine) = simulateMachine(List(Coin)).run(machine)

      candies shouldBe 2
      coins shouldBe 4
      newMachine.locked shouldBe false
    }

    it("an unlocked machine that has no candy does not accept coins") {
      val machine = CandyMachine(true, 0, 4)
      val ((candies, coins), newMachine) = simulateMachine(List(Coin)).run(machine)

      candies shouldBe 0
      coins shouldBe 4
      newMachine.locked shouldBe true
    }

    it("should do nothing when turing the knob on a unlocked machine without candy") {
      val machine = CandyMachine(false, 0, 4)
      val ((candies, coins), newMachine) = simulateMachine(List(Turn)).run(machine)

      candies shouldBe 0
      coins shouldBe 4
      newMachine.locked shouldBe false
    }

    it("should respond to a sequence of actions") {
      val machine = CandyMachine(true, 5, 10)
      val ((candies, coins), newMachine) = simulateMachine(List(Coin,Turn,Turn,Coin,Turn,Coin,Coin,Turn,Coin,Turn)).run(machine)

      candies shouldBe 1
      coins shouldBe 14
    }
  }
}
