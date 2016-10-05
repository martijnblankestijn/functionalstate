import scala.annotation.tailrec

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def process(input: Input): Machine =
    if (candies == 0) this
    else
      input match {
        case Coin => insertCoin(1)
        case Turn => turnKnob()
      }

  private def insertCoin(inserted: Int): Machine =
    if (!locked) this else copy(locked = false, coins = coins + inserted)

  private def turnKnob(): Machine =
    if (locked) this else copy(locked = true, candies = candies - 1)
}

case class State[S, +A](run: S => (A, S))

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(machine => {
      val endState = inputs.foldLeft(machine)((m, input) => m.process(input))
      ((endState.coins, endState.candies), endState)
    })


  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val run = (m: Machine) => inputs.foldLeft(((0, 0), m))((m, input) => {
      val newState: Machine = m._2.process(input)
      ((newState.coins, newState.candies), newState)
    })
    State(run)
  }
  
  def simulateMachine3(start: Machine)(inputs: List[Input]): Machine = {
    
    @tailrec
    def loop(inputs: List[Input], result: Machine): Machine = inputs match {
      case Nil     => result
      case x :: xs => loop(xs, result.process(x))
    }
    
    loop(inputs, start) 
  }
}