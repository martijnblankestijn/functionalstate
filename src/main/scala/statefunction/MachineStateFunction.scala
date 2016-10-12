package statefunction

import domain.Input
import state.Machine

class MachineStateFunction
object MachineStateFunction {
  def input(input: Input, m: Machine): (Machine, (Int, Int)) = {
      val m1 = m.process(input)
      (m1, (m1.coins, m1.candies))
    }

  def refill(newCandies: Int, m: Machine): (Machine, (Int, Int)) = {
      val m1 = m.copy(locked = true, newCandies + m.candies)
      //      println("Refilled machine, now " + result + " for machine " + m1)
      (m1, (m1.coins, m1.candies))
    }

  def collect(m: Machine): (Machine, (Int, Int)) = {
      val m1 = m.copy(locked = true, coins = 0)
      (m1, (m1.coins, m1.candies))
    }

}