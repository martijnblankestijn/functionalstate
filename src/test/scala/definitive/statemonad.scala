package definitive

import definitive.Candies.candies
import definitive.StateMonad.{Machine, State}
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.Seq
object StateMonad {

    class State[S, +A](f: S => (S, A)) {
      def run(initial: S): (S, A) = f(initial)

      def map[B](transform: A => B): State[S, B] =
        State[S, B](s0 => {
          val (s1, a) = run(s0)
          (s1, transform(a))
        })

      def flatMap[B](g: A => State[S, B]): State[S, B] =
        State(s0 => {
          val (s1, a) = run(s0)
          g(a).run(s1)
        })
    }

    object State {
      def apply[S,A](f: S => (S,A)) = new State(f)
      def unit[S, A](a: A): State[S, A] = new State(s => (s, a))

      def get[S]: State[S, S] = new State(s => (s, s))

      def set[S](newS: S): State[S, Unit] = new State(_ => (newS, ()))

      def modify[S](f: S => S): State[S, Unit] = new State(s => (f(s), ()))
    }

case class Machine(coins: Int, candies: List[Candy])

object Machine {
  def insertCoin(coin: Coin): State[Machine, Unit] =
    State(m => (m.copy(coins = m.coins + 1), ()))

  def turn(): State[Machine, Candy] =
    State(m => (m.copy(candies = m.candies.tail), m.candies.head))

      def insertCoinSet(coin: Coin): State[Machine, Unit] = for {
        m <- State.get[Machine]
        _ <- State.set(m.copy(coins = m.coins + 1))
      } yield ()

      def insertCoinModify(coin: Coin): State[Machine, Unit] = for {
        _ <- State.modify[Machine](m => m.copy(coins = m.coins + 1))
      } yield ()

    }
}
class StateMonadSpec extends WordSpec with Matchers {
  import Machine._
  "A vending Machine" should {
    "turn as our first refactoring" in {
// Go, go, go ...
val m0 = Machine(0, candies)

val t: State[Machine, Candy] = Machine.turn()
val (m1, candy) = t run m0

candy shouldBe Candy("Blue")
      // nothing done to insertCoin so no Coins available
    }

    "use of map split" in {
val m0 = Machine(0, candies)

val turn: State[Machine, Candy] = Machine.turn()
val tmap: State[Machine, String] = turn.map(_.color)
val (m1, color) = tmap.run(m0)

color shouldBe "Blue"
      // nothing done to insertCoin so no Coins available
    }
    
    " use of map combined" in {
val m0 = Machine(0, candies)

val state = Machine.turn().map(_.color)
val (m1, color) = state.run(m0)
      
color shouldBe "Blue"
    }


    " use of flatMap" in {
val m0 = Machine(0, candies)

val program = Machine.insertCoin(Coin).flatMap(_ => turn())
val (m1, candy) = program.run(m0)

candy shouldBe Candy("Blue")
    }

    " use of multiple flatMaps" in {
      val m0 = Machine(0, candies)

val program =
  Machine.insertCoin(Coin)
    .flatMap(_ => Machine.turn()
      .flatMap(_ => Machine.insertCoin(Coin)
        .flatMap(_ => Machine.turn())))
val (m1, candy) = program.run(m0)

candy shouldBe Candy("Red")
    }

    " use of for comprehension simplifies." in {
      val m0 = Machine(0, candies)

val program = for {
  _ <- insertCoin(Coin)
  _ <- turn()
  _ <- insertCoin(Coin)
  candy <- turn()
} yield candy

val (m1, candy) = program.run(m0)

candy shouldBe Candy("Red")
    }


    "unlock when coin inserted and candy available" in {
      // this is just pipelining (threading through) of the state
      // this does not do anything
      val program3 = for {
        _ <- insertCoin(Coin)
        _ <- turn()
        _ <- insertCoin(Coin)
        m <- turn()
      } yield m
      
      val (machine, candy) = program3.run(Machine(candies = candies, coins = 2))
      candy shouldBe Candy("Red")
      
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
      
      val bigMachine = Machine(candies = candies ++ candies ++ candies, coins = 0)
      // changing the state, say you want to change the machine
      val refillMachine =State.set(bigMachine) 
      
      val changedState: (Machine, Unit) = refillMachine.run(machine)
      println("Used set: " + changedState)
      changedState._1.candies.size shouldBe candies.size * 3
      changedState._1.coins shouldBe 0
      
      val v: Seq[State[Machine, Candy]] = 1 to 10 map { _ =>
        for {
          _ <- insertCoin(Coin)
          m <- turn()
        } yield m
      }
      val runNTimes: State[Machine, Candy] = v reduce { (state0: State[Machine,Candy], state1: State[Machine,Candy]) =>
        for {
          _ <- state0
          m <- state1
        } yield m
      }
      
      val (m,_) = refillMachine.flatMap(_ => runNTimes).run(bigMachine)
      println(m)
      m.coins shouldBe 10
      m.candies.size shouldBe candies.size * 3 - 10  

      
    }
  }
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  