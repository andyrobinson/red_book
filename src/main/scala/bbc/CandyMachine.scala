package bbc
import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

//definitely not the most compact you could make it, but understandable

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {

  private def machineStateTransition(ip:Input): CandyMachine => CandyMachine = {
    mc => ip match {
      case Coin if (mc.locked && mc.candies > 0) => CandyMachine(false, mc.candies, mc.coins + 1)
      case Turn if (!mc.locked && mc.candies > 0) => CandyMachine(true, mc.candies - 1, mc.coins)
      case _ => mc
    }
  }

  def simulateMachine(inputs: List[Input]): State[CandyMachine, (Int, Int)] = {
    State(s => {
      val states = inputs map (x => modify[CandyMachine] (machineStateTransition(x)))
      val (_,mc) = sequence(states).run(s)
      ((mc.candies, mc.coins), mc)
    })
  }
}