package sideeffecting

import org.scalatest.{Matchers, WordSpec}

class SideEffectingSpec extends WordSpec with Matchers {

  "Side effect free" should {
    "substitution model " in {
      val x = 2

      def f(x: Int): Int = x * 5
      f(x) shouldBe f(x)
    }



    "mutatability is wrong" in {
      val x = 2

      var y = 0
      def g(x: Int): Int = {
        y = y + x
        y
      }

      // DOES NOT HOLD
      // g(x) shouldBe g(x)

    }
  }

}
