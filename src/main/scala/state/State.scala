package state

/**
  * Created by mblankestijn on 12/10/16.
  */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s1) = run(s)
    (f(a), s1)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

  /**
    * What does filter mean with a State Monad??
    */
  /*  
    def filter(f: A => Boolean): State[S,A] =
      State { s =>
        val (a, s1) = run(s)
  //      if(f(a))   ...??
        (a, s1)
      }
      */

}
