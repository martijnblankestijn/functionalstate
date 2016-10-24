package definitive

import scala.collection.mutable.ListBuffer

sealed trait Coin
case object Coin extends Coin

case class Candy(color: String)

object Candies {
  val candies: List[Candy] =
    List("Blue", "Red", "White", "Purple", "Green", "Orange", "Pink", "Yellow")
      .map(Candy)
  def mutCandies: ListBuffer[Candy] = ListBuffer(candies: _*)

}


