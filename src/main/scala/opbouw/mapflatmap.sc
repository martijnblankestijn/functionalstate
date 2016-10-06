import scalaz.Alpha.B

// Hoe komen we van een State[S,A] naar een State[S,B]
// oftwel hoe implementeren we map
trait StateX[S, +A] {
  def run(initial: S): (S,A)
  def map[B](f: A => B): StateX[S, B] =
    StateX { s => 
      val (s1, a) = run(s)
      (s1, f(a))
  }

  def flatMap[B](f: A => StateX[S, B]): StateX[S, B] =
    StateX { s =>
      val (s1, a) = run(s)
      f(a).run(s1)
    }
}

object StateX {
  def apply[S,A](f: S => (S,A)): StateX[S, A] =
    new StateX[S,A] {
      override def run(i: S) = f(i)
    }
}
println("HALLO")
val s = StateX( (s: Int) => (s + 1,"XX" + s))
println("Nothing done")
s.run(7)

val composed: StateX[Int, Int] = s map {a => a.length}
composed.run(9)


