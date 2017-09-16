package mutable

import domain.{Coin, Input, Turn}
import org.scalatest.{Matchers, WordSpec}

class MutableMachineSpec extends WordSpec with Matchers {
  "A vending Machine" should {
    "unlock when coin inserted and candy available" in {
      val machine = OriginalMachine(locked = true, candies = 1, coins = 0)
      machine.process(Coin)

      machine.candies shouldBe 1
      machine.locked shouldBe false
      machine.coins shouldBe 1
    }

    "turn on an unlocked machine will dispense candy and lock" in {
      val machine = OriginalMachine(locked = false, candies = 1, coins = 0)
      machine.process(Turn)

      machine.candies shouldBe 0
      machine.locked shouldBe true
      machine.coins shouldBe 0
    }

    "do nothing on turning the knob on a locked machine or inserting a coin into an unlocked machine" in {
      val lockedMachine = OriginalMachine(locked = true, candies = 1, coins = 0)
      lockedMachine.process(Turn)
      lockedMachine.locked shouldBe true

      val unlockedMachine = OriginalMachine(locked = false, candies = 1, coins = 0)
      unlockedMachine.process(Coin)
      unlockedMachine.locked shouldBe false
    }

    "ignore all input when out of candy" in {
      val unlockedMachine = OriginalMachine(locked = false, candies = 0, coins = 0)
      unlockedMachine.process(Turn)
      unlockedMachine.locked shouldBe false
      unlockedMachine.coins shouldBe 0
      unlockedMachine.candies shouldBe 0
    //   missing test for Coin

      val lockedMachine = OriginalMachine(locked = true, candies = 0, coins = 0)
      lockedMachine.process(Turn)

      lockedMachine.locked shouldBe true
      lockedMachine.coins shouldBe 0
      lockedMachine.candies shouldBe 0
      //   missing test for Coin
    }


    "on input and turning delivers a candy" in {
val machine = OriginalMachine(locked = true, candies = 5, coins = 10)

machine process Coin
machine process Turn

machine.coins shouldBe 11
machine.candies shouldBe 4
    }

    "with 5 candies and 10 coins and refill" in {
      val inputs: List[Input] = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

      val machine = OriginalMachine(locked = true, candies = 5, coins = 10)
      for (i <- inputs) machine process i
      machine.refill(100 - machine.candies)
      machine.collect()

      machine.locked shouldBe true
      machine.coins shouldBe 0
      machine.candies shouldBe 100

    }

  }
}
