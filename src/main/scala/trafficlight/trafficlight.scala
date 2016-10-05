package trafficlight

// From http://timperrett.com/2013/11/25/understanding-state-monad/


case class Signal(isOperational: Boolean, display: Map[TrafficLight, Mode])

sealed trait TrafficLight
case object Red extends TrafficLight
case object Orange extends TrafficLight
case object Green extends TrafficLight

sealed trait Mode
case object Off extends Mode
case object Flashing extends Mode
case object Solid extends Mode

object Signal {

  import scalaz.State
  import scalaz.State._

  type ->[A, B] = (A, B)
  type SignalState[A] = State[Signal, A]

  val default = Signal(
    isOperational = false,
    display = Map(Red -> Flashing, Orange -> Off, Green -> Off))

  def enable: State[Signal, Boolean] =
    for {
      a <- init
      _ <- modify((s: Signal) => s.copy(isOperational = true))
      r <- get
    } yield r.isOperational
}