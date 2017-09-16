package definitive

import definitive.Traditional.Machine
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Traditional {
  
class Machine(private val candies: mutable.Buffer[Candy],
              private var coins: Int) {
  
  def insert(coin: Coin): Unit = 
    coins = coins + 1
  
  def turn(): Candy =  candies.remove(0)

  def getCoins = coins
  def remainingCandies = candies.size
}

}
class TraditionalSpec extends WordSpec with Matchers {

  "A vending Machine" should {

    "can dispense a Candy" in {
val candies = ArrayBuffer(
  Candy(BLUE), 
  Candy(RED), 
  Candy(GREEN))

val machine = new Machine(candies, 0)

machine.insert(Coin())
val candy: Candy = machine.turn()

candy shouldBe Candy(BLUE)
machine.getCoins shouldBe 1
machine.remainingCandies shouldBe 2
    }

    "can be called multiple times" in {
      val candies = ArrayBuffer(Candy(BLUE), Candy(RED), Candy(GREEN))
      val machine = new Machine(candies, 0)

      candies.toList.foreach { c =>
        machine.insert(Coin())
        machine.turn() shouldBe c
      }
      machine.getCoins shouldBe 3
      machine.remainingCandies shouldBe 0
    }
  }
}
