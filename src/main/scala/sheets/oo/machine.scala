package sheets.oo

import scala.collection.mutable

sealed trait Color
case object BLUE extends Color
case object RED extends Color
case object GREEN extends Color

case class Coin()
case class Candy(color:Color)

class Machine(private val candies: mutable.Buffer[Candy],
              private var coins: Int) {
  def insert(coin: Coin): Unit = coins = coins + 1
  def turn(): Candy = candies.remove(0)
  def getCoins = coins
}