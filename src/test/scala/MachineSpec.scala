import org.scalatest.{Matchers, WordSpec}

class MachineSpec extends WordSpec with Matchers {
  "A vending Machine" should {
    "unlock when coin inserted and candy available" in {
      val machine = Machine(locked = true, candies = 1, coins = 0)
      val newMachine: Machine = machine.process(Coin)

      newMachine.candies shouldBe 1
      newMachine.locked shouldBe false
      newMachine.coins shouldBe 1
    }

    "turn on an unlocked machine will dispense candy and lock" in {
      val machine = Machine(locked = false, candies = 1, coins = 0)
      val newMachine: Machine = machine.process(Turn)

      newMachine.candies shouldBe 0
      newMachine.locked shouldBe true
      newMachine.coins shouldBe 0
    }

    "do nothing on turning the knob on a locked machine or inserting a coin into an unlocked machine" in {
      val lockedMachine = Machine(locked = true, candies = 1, coins = 0)
      lockedMachine.process(Turn) shouldBe lockedMachine

      val unlockedMachine = Machine(locked = false, candies = 1, coins = 0)
      unlockedMachine.process(Coin) shouldBe unlockedMachine
    }

    "ignore all input when out of candy" in {
      val unlockedMachine = Machine(locked = false, candies = 0, coins = 0)
      unlockedMachine.process(Turn) shouldBe unlockedMachine
      unlockedMachine.process(Coin) shouldBe unlockedMachine

      val lockedMachine = Machine(locked = true, candies = 0, coins = 0)
      lockedMachine.process(Turn) shouldBe lockedMachine
      lockedMachine.process(Coin) shouldBe lockedMachine
    }


    "with 5 candies and 10 coins" in {
      val initialMachine: Machine = Machine(locked = true, candies = 5, coins = 10)
      val state: State[Machine, (Int, Int)] = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      val ((coins, candies), m) = state.run(initialMachine)

      m.locked shouldBe true
      coins shouldBe 14
      candies shouldBe 1

    }

    "with 5 candies and 10 coins and refill" in {
      val initialMachine: Machine = Machine(locked = true, candies = 5, coins = 10)
      val endState =
        Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
          .flatMap { case (_, cand) => Machine.refill(100 - cand) }
      val ((coins, candies), m) = endState.run(initialMachine)

      m.locked shouldBe true
      coins shouldBe 0
      candies shouldBe 100

    }


    "with 5 candies and 10 coins recursive" in {
      val initialMachine: Machine = Machine(locked = true, candies = 5, coins = 10)
      val endState: Machine = Machine.simulateMachine3(initialMachine)(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))

      endState.locked shouldBe true
      endState.coins shouldBe 14
      endState.candies shouldBe 1

    }

  }
}
