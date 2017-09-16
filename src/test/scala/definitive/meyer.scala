package definitive

import definitive.Candies.mutCandies
import definitive.Meyer.Machine
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Meyer {

class Machine(private val candies: mutable.Buffer[Candy],
              private var _coins: Int) {
  private var _lastCandy:Candy = _

  // 'Commands' modify machine
  def insert(coin: Coin): Unit = _coins = _coins + 1

  def turn(): Unit = _lastCandy = candies.remove(0)
  

  // 'Queries' return information about machine
  def lastCandy = _lastCandy
  def remainingCandies = candies.size
  def coins = _coins
}

}
class MeyerSpec extends WordSpec with Matchers {
  "A vending Machine" should {
    "unlock when coin inserted and candy available" in {
      val machine = new Machine(mutCandies, 0)

      mutCandies.foreach { c =>
        machine.insert(Coin())
        machine.turn()

        machine.lastCandy shouldBe c
      }
      machine.coins shouldBe 3
      machine.remainingCandies shouldBe 0
      
      // Note no introduction of option
//
//      machine.insert(Coin)
//      machine.turn()
//
//      machine.lastCandy shouldBe None
    }
  }
}
