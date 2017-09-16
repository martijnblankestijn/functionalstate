package mutable


import domain.{Coin, Input, Turn}

class OriginalMachine(private var _locked: Boolean,
                      private var _candies: Int,
                      private var _coins: Int) {

  def process(input: Input): Unit = {
    if (_candies > 0) input match {
      case Coin => insert(1)
      case Turn => turn()
    }
  }

  def collect(): Unit = 
    _coins = 0
  

  def refill(newCandies: Int): Unit = 
    _candies = _candies + newCandies

  // Expose read-only variables
  def candies = _candies
  def coins = _coins
  def locked = _locked

  private def insert(inserted: Int) =
    if (_locked) {
      _locked = false
      _coins = _coins + inserted
    }

  private def turn() =
    if (!_locked) {
      _locked = true
      _candies = _candies - 1
    }
}

object OriginalMachine {
  def apply(locked: Boolean, candies: Int, coins: Int) = new OriginalMachine(locked, candies, coins)
}