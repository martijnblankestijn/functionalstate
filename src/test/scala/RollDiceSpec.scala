import RNG._
import org.scalatest.{Matchers, WordSpec}

class RollDiceSpec extends WordSpec with Matchers {
  "Roll dice imperatively" should {
    "expect valid dice" in {
      val beValidDice = be >= 0 and be <= 6
      rollDice should beValidDice
    }

    "show error" in {
      val is = (1 to 10000).map(_ => rollDice).toSet
      println(is)
    }
  }
  "Roll dice functional" should {
"show referential transparency" in {
  val original: RNG = SimpleRNG(seed = 12345)
  
  val (value, nextRng) = original.nextInt
  (value, nextRng) shouldBe original.nextInt
}
    "expect valid dice" in {
      val rng: RNG = SimpleRNG(seed = 12345)
      val (v2, rng2) = rollDiceF(rng)
      val (v3, rng3) = rollDiceF(rng)
      
      v2 shouldBe v3
      rng2 shouldBe rng3
    }
  }
  
  def rollDiceF(rng: RNG) = {
    val (value, rng2) = rng.nextInt
    (Math.abs(value % 6), rng2)
  }
}

