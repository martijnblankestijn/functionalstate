class MutableMachine(private var _locked: Boolean,
                     private var _candies: Int,
                     private var _coins: Int) {

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

object MutableMachine {
  def apply(locked: Boolean, candies: Int, coins: Int) = new MutableMachine(locked, candies, coins)

  def simulateMachine(inputs: List[Input], machine: MutableMachine): Unit = inputs foreach(machine process _)


  def simulateMachineCopied(inputs: List[Input]): State[MutableMachine, (Int, Int)] =
    State(machine => {
      val endState = inputs.foldLeft(machine)((m, input) => {
        m.process(input)
        m
      })
      ((endState.coins, endState.candies), endState)
    })

}