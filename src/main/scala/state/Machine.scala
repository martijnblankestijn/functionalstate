package state

import domain.{Coin, Turn}

import scala.annotation.tailrec

/**
  * Created by mblankestijn on 12/10/16.
  */
case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def process(input: domain.Input): Machine =
    if (candies == 0) this
    else
      input match {
        case Coin => insert(1)
        case Turn => turnKnob()
      }

  private def insert(inserted: Int): Machine =
    if (!locked) this else copy(locked = false, coins = coins + inserted)

  private def turnKnob(): Machine =
    if (locked) this else copy(locked = true, candies = candies - 1)
}

object Machine {
  def simulateMachine(inputs: List[domain.Input]): State[Machine, (Int, Int)] =
    State(machine => {
      val endState = inputs.foldLeft(machine)((m, input) => m.process(input))
      ((endState.coins, endState.candies), endState)
    })

def input(input: domain.Input): State[Machine, (Int, Int)] =
  State { m =>
    val m1 = m.process(input)
    ((m1.coins, m1.candies), m1)
  }

  def maintain(candies: Int): State[Machine, (Int, Int)] =
    refill(candies)
      .flatMap(m => collect())

  def refill(newCandies: Int): State[Machine, (Int, Int)] =
    State { m =>
      val m1 = m.copy(locked = true, newCandies + m.candies)
      //      println("Refilled machine, now " + result + " for machine " + m1)
      ((m1.coins, m1.candies), m1)
    }

  def collect(): State[Machine, (Int, Int)] =
    State { m =>
      val m1 = m.copy(locked = true, coins = 0)
      ((m1.coins, m1.candies), m1)
    }


def maintainForComprehension(candies: Int): State[Machine, (Int, Int)] =
  for {
    _ <- refill(candies)
    s <- collect()
  } yield s

  def simulateMachine2(inputs: List[domain.Input]): State[Machine, (Int, Int)] = {
    val run = (m: Machine) => inputs.foldLeft(((0, 0), m))((m, input) => {
      val newState: Machine = m._2.process(input)
      ((newState.coins, newState.candies), newState)
    })
    State(run)
  }

  def simulateMachine3(start: Machine)(inputs: List[domain.Input]): Machine = {

    @tailrec
    def loop(inputs: List[domain.Input], result: Machine): Machine = inputs match {
      case Nil => result
      case x :: xs => loop(xs, result.process(x))
    }

    loop(inputs, start)
  }
}