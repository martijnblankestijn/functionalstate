import org.scalatest.{Matchers, WordSpec}
import RNG._

class SimpleRNGSpec extends WordSpec with Matchers{
  "Simple RNG " should {
    "list ints" in {
      val (ints, rngs) = RNG.ints(3)(SimpleRNG(23L))
      println(ints)
    }
  }

  "both" in {
    val ((i,d), rng) = both(nonNegativeInt, double)(SimpleRNG(1))
    println(i + " " + d)
  }

}
