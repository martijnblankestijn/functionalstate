package statemonad

import org.scalatest.{Matchers, WordSpec}
import statemonad.StateMonad.{Candy, Coin, Machine, State}
import statemonad.StateMonad.Machine._

class StateMonadSpec extends WordSpec with Matchers {
  "A vending Machine" should {
    "unlock when coin inserted and candy available" in {
      // this is just pipelining (threading through) of the state
      // this does not do anything
      val program3 = for {
        _ <- insert(Coin())
        _ <- turn()
        _ <- insert(Coin())
        m <- turn()
      } yield m
      
      val (machine, candy) = program3.run(Machine(candies = 10, coins = 2))
      candy shouldBe Candy()
      
      val getTheCoins: State[Machine, Int] = for {
        m <- State.get[Machine]
        c = {
          println("Inside the for-comprehension: Coins: " + m.coins)
          m.coins
        }
      } yield c
      
      println("Before running getTheCoins")
      getTheCoins.run(machine)._2 shouldBe 4
      println("After running getTheCoins")
      
      val bigMachine = Machine(candies = 100, coins = 0)
      // changing the state, say you want to change the machine
      val modified = for {
        _ <- State.get[Machine]
        c <- State.set(bigMachine)
      } yield c
      
      val changedState: (Machine, Unit) = modified.run(machine)
      println("Used set: " + changedState)
      changedState._1.candies shouldBe 100
      changedState._1.coins shouldBe 0
    }
  }
}


object StateMonad {

  class State[S, +A](f: S => (S, A)) {
    def run(initial: S): (S, A) = f(initial)

def map[B](transform: A => B): State[S, B] =
  new State[S, B](s0 => {
    val (s1, a) = run(s0)
    (s1, transform(a))
  })

def flatMap[B](g: A => State[S, B]): State[S, B] =
  new State(s0 => {
    val (s1, a) = run(s0)
    g(a).run(s1)
  })
  }

  object State {
    def apply[S, A](a: A): State[S, A] = new State(s => (s, a))

    def get[S]: State[S, S] = new State(s => (s, s))

    def set[S](newS: S): State[S, Unit] = new State(_ => (newS, ()))

    def modify[S](f: S => S): State[S, Unit] = new State(s => (f(s), ()))
  }

  case class Candy()
  case class Coin()

  case class Machine(candies: Int, coins: Int)

  object Machine {
    def insert(coin: Coin): State[Machine, Unit] =
      new State(machine =>
        (machine.copy(coins = machine.coins + 1), ())
      )

    def turn(): State[Machine, Candy] =
      new State(machine =>
        (machine.copy(candies = machine.candies - 1), Candy())
      )

    def insertSet(coin: Coin): State[Machine, Unit] = for {
      m <- State.get[Machine]
      _ <- State.set(m.copy(coins = m.coins + 1))
    } yield ()

    def insertModify(coin: Coin): State[Machine, Unit] = for {
      _ <- State.modify[Machine](m => m.copy(coins = m.coins + 1))
    } yield ()

  }

  val program = Machine.turn().map(candies => s"There are $candies left")
  val (m, c) = program.run(Machine(candies = 100, coins = 0))

  val program2 =
    Machine.insert(Coin())
      .flatMap(_ => Machine.turn()
        .flatMap(_ => Machine.insert(Coin())
          .flatMap(_ => Machine.turn())
        )
      )
  val (m1, c1) = program2.run(Machine(candies = 100, coins = 0))
  assert(m1.coins == 2)

  val program3 = for {
    _ <- Machine.insert(Coin())
    _ <- Machine.turn()
    _ <- Machine.insert(Coin())
    c <- Machine.turn()
  } yield c

}