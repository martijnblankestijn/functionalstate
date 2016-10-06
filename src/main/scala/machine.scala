import scala.annotation.tailrec

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def maintain(newCandies: Int): Machine = Machine(locked = true, newCandies + candies, coins = 0)
  
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

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B) : State[S,B] = State { s =>
    val (a,s1) = run(s)
    (f(a), s1)
  }
  
  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)  
    }
}

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(machine => {
      val endState = inputs.foldLeft(machine)((m, input) => m.process(input))
      ((endState.coins, endState.candies), endState)
    })
  
  def refill(candies: Int): State[Machine, (Int, Int)] = 
    State(machine => {
      val machine1: Machine = machine.maintain(candies)
      val result: (Int, Int) = (machine1.coins, machine1.candies)
      println("Refilled machine, now " + result + " for machine " + machine1)
      (result, machine1)
    }
    )


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