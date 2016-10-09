import org.scalatest.{Matchers, WordSpec}

import scalaz.Alpha.M

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


    val inputs: List[Input with Product with Serializable] = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    
    "with 5 candies and 10 coins" in {
      val initialMachine: Machine = Machine(locked = true, candies = 5, coins = 10)
      val state: State[Machine, (Int, Int)] = Machine.simulateMachine(inputs)
      val ((coins, candies), m) = state.run(initialMachine)

      m.locked shouldBe true
      coins shouldBe 14
      candies shouldBe 1

    }

    "with 5 candies and 10 coins and refill" in {
      val initialMachine: Machine = Machine(locked = true, candies = 5, coins = 10)
      
      val m1: Machine = initialMachine.process(Coin)
      val m2 = m1.process(Turn)
      val m3 = m2.refill(newCandies = 5)
      val m4 = m3.collect()
      val m5 = m4.process(Coin)
      
      println("Machines:\n" + initialMachine + "\n" + m1 + "\n" + m2+ "\n" + m3+ "\n" + m4 + "\n" + m5)
      
//      val endState =
//        Machine.simulateMachine(inputs)
//          .flatMap { case (_, cand) => Machine.refill(100 - cand) }

      val endState: State[Machine, (Int, Int)] = for {
        // http://stackoverflow.com/questions/4380831/why-does-filter-have-to-be-defined-for-pattern-matching-in-a-for-loop-in-scala
        // COMPILER ERROR:
//        (_, candies) <- Machine.simulateMachine(inputs)
//              r <- Machine.refill(100 - candies)
        s <- Machine.simulateMachine(inputs)
//        _ <- Machine.refill(100 - s._2)
//        t <- Machine.collect()
        t <- Machine.maintain(100 - s._2)

      } yield t

      val ((coins, candies), m) = endState.run(initialMachine)
      
      m.locked shouldBe true
      coins shouldBe 0
      candies shouldBe 100

    }


    "with 5 candies and 10 coins recursive" in {
      val initialMachine: Machine = Machine(locked = true, candies = 5, coins = 10)
      val endState: Machine = Machine.simulateMachine3(initialMachine)(inputs)

      endState.locked shouldBe true
      endState.coins shouldBe 14
      endState.candies shouldBe 1

    }

  }
}
