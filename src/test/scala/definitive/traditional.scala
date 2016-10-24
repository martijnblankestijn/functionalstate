package definitive

import definitive.Candies.{candies, mutCandies}
import definitive.Traditional.Machine
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ListBuffer

object Traditional {

class Machine(private val candies: ListBuffer[Candy],
              private var _coins: Int) {
  
  def insertCoin(coin: Coin): Unit = _coins = _coins + 1
  def turn(): Candy = candies.remove(0)

  def remainingCandies = candies.size
  def coins = _coins
}

}
class TraditionalSpec extends WordSpec with Matchers {

  "A vending Machine" should {

    "can dispense a Candy" in {
val machine = new Machine(mutCandies, 0)

machine.insertCoin(Coin)
val candy: Candy = machine.turn()

candy shouldBe Candy("Blue")
machine.coins shouldBe 1
machine.remainingCandies shouldBe candies.size - 1
    }

    "can be called multiple times" in {
      val machine = new Machine(mutCandies, 0)

      mutCandies.foreach { c =>
        machine.insertCoin(Coin)
        machine.turn() shouldBe c
      }
      machine.coins shouldBe 8
      machine.remainingCandies shouldBe 0
    }
  }
}
