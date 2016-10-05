import org.scalatest.{Matchers, WordSpec}
import MutableMachine._

class MutableMachineSpec extends WordSpec with Matchers {
  "A vending Machine" should {
    "unlock when coin inserted and candy available" in {
      val machine = MutableMachine(locked = true, candies = 1, coins = 0)
      machine.process(Coin)

      machine.candies shouldBe 1
      machine.locked shouldBe false
      machine.coins shouldBe 1
    }

    "turn on an unlocked machine will dispense candy and lock" in {
      val machine = MutableMachine(locked = false, candies = 1, coins = 0)
      machine.process(Turn)

      machine.candies shouldBe 0
      machine.locked shouldBe true
      machine.coins shouldBe 0
    }

    "do nothing on turning the knob on a locked machine or inserting a coin into an unlocked machine" in {
      val lockedMachine = MutableMachine(locked = true, candies = 1, coins = 0)
      lockedMachine.process(Turn)
      lockedMachine.locked shouldBe true

      val unlockedMachine = MutableMachine(locked = false, candies = 1, coins = 0)
      unlockedMachine.process(Coin)
      unlockedMachine.locked shouldBe false
    }

    "ignore all input when out of candy" in {
      val unlockedMachine = MutableMachine(locked = false, candies = 0, coins = 0)
      unlockedMachine.process(Turn)
      unlockedMachine.locked shouldBe false
      unlockedMachine.coins shouldBe 0
      unlockedMachine.candies shouldBe 0
    //   missing test for Coin

      val lockedMachine = MutableMachine(locked = true, candies = 0, coins = 0)
      lockedMachine.process(Turn)

      lockedMachine.locked shouldBe true
      lockedMachine.coins shouldBe 0
      lockedMachine.candies shouldBe 0
      //   missing test for Coin
    }


    "with 5 candies and 10 coins" in {
      val initialMachine: MutableMachine = MutableMachine(locked = true, candies = 5, coins = 10)
//      val state: State[MutableMachine, (Int, Int)] = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      val inputs: List[Input] = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      simulateMachine(inputs, initialMachine)

      initialMachine.locked shouldBe true
      initialMachine.coins shouldBe 14
      initialMachine.candies shouldBe 1

    }
  }
}
