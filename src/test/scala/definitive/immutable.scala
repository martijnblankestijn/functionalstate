package definitive

import definitive.Candies.candies
import definitive.Immutable.Machine
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable

object Immutable {
  
  
case class Machine(candies: immutable.List[Candy], coins: Int)

  
object Machine {
  
  def turn(m: Machine): (Machine, Candy) = 
    ( m.copy(candies = m.candies.tail), m.candies.head )

  def insert(coin: Coin, m: Machine): (Machine) =
    m.copy(coins = m.coins + 1)
}


}

class ImmutableSpec extends WordSpec with Matchers {
  "A vending Machine" should {
    "handle multiple inserts and turns" in {
val candies = List(Candy(BLUE), Candy(RED), Candy(GREEN))
val m0 = Machine(candies, 0)

val m1:Machine   = Machine.insert(Coin(), m0)
val (m2, candy0) = Machine.turn(m1)

val m3           = Machine.insert(Coin(), m2)
val (m4, candy1) = Machine.turn(m3)

m4.candies.size shouldBe 1
m4.coins shouldBe 2
candy1 shouldBe Candy(RED)

candy0 shouldBe Candy(BLUE)
m2.coins shouldBe 1
    }
  }
}
