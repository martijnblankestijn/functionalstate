package mutable


import domain.{Coin, Input, Turn}

class Machine(private var _locked: Boolean,
              private var _candies: Int,
              private var _coins: Int) {
  def collect(): Unit = 
    _coins = 0
  

  def refill(newCandies: Int): Unit = 
    _candies = _candies + newCandies


  def process(input: Input): Unit = {
    if (_candies > 0) input match {
      case Coin => insertCoin(1)
      case Turn => turnKnob()
    }
  }

  // Expose read-only variables
  def candies = _candies
  def coins = _coins
  def locked = _locked

  private def insertCoin(inserted: Int) =
    if (_locked) {
      _locked = false
      _coins = _coins + inserted
    }

  private def turnKnob() =
    if (!_locked) {
      _locked = true
      _candies = _candies - 1
    }
}

object Machine {
  def apply(locked: Boolean, candies: Int, coins: Int) = new Machine(locked, candies, coins)
}