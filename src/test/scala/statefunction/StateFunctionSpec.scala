package statefunction

import domain.{Coin, Input, Turn}
import org.scalatest.{Matchers, WordSpec}
import state.Machine
import statefunction.MachineStateFunction._

/**
  * Created by mblankestijn on 12/10/16.
  */
class StateFunctionSpec extends WordSpec with Matchers {
  "A vending Machine" should {
    "with 5 candies and 10 coins and refill" in {
      val machine = Machine(locked = true, candies = 5, coins = 10)
      val (m1, _) = input(Coin, machine)
      val (m2, (_, candies2)) = input(Turn, m1)
      val (m3: Machine, _) = refill(candies2, m2)

      m3.coins shouldBe 11
      m3.candies shouldBe 8

      val (m4, (coins, candies)) = collect(m3)

      coins shouldBe 0
      candies shouldBe 8

    }
  }
}
