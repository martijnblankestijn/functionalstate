package definitive

import scala.collection.mutable.ArrayBuffer

sealed trait Color
case object BLUE extends Color
case object RED extends Color
case object GREEN extends Color


case class Coin()

case class Candy(color: Color)

object Candies {
  val candies: List[Candy] =
    List(BLUE, RED, GREEN)
      .map(Candy)
  def mutCandies: ArrayBuffer[Candy] = ArrayBuffer(candies: _*)

}


