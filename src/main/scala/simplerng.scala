import scala.Int.MaxValue
import scala.collection.immutable.IndexedSeq

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt = {
    val newSeed = calculateNewSeed()
    (calculateNewRandom(newSeed), SimpleRNG(newSeed))
  }

  private def calculateNewSeed() = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFL

  private def calculateNewRandom(newSeed: Long) = (newSeed >>> 16).toInt
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng)
    val (d3, rng3) = double(rng)
    ((d1, d2, d3), rng3)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val op: ((List[Int], RNG), Int) => (List[Int], RNG) = {
      case ((l, r), i) =>
        val (integer, rgnnext) = r.nextInt
        println(s"From $integer: $rgnnext")
        (l :+ integer, rgnnext)
    }
    val left: (List[Int], RNG) = (0 until count).foldLeft( (List[Int](), rng) )(op)
    left
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_ / (MaxValue.toDouble + 1))(rng)
  }

  def doubleOld(rng: RNG): (Double, RNG) = {
    val (r, nextRng) = nonNegativeInt(rng)
    (r / (MaxValue.toDouble + 1), nextRng)
  }

//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//    fs.foldRight( unit(fs.head)) ( () )
//    rng => if(fs isEmpty) (List(), rng) else {
//      val (i, rng1) = fs.head(rng)
//      fs.tail
//      val  = head.nextInt
      //sequence(fs.tail)
//      ???
//    }
//  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    def makePositive(i: Int) = if (i < 0) -(i + 1) else i

    val (nextRandom, newRng) = rng.nextInt
    (makePositive(nextRandom), newRng)
  }
}
