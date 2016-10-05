import RNG._

import scala.collection.immutable.IndexedSeq

def newRng(i: Int) = new RNG {
  override def nextInt = (i, SimpleRNG(0))
}

nonNegativeInt(newRng(Int.MinValue))
nonNegativeInt(newRng(Int.MinValue + 1))
nonNegativeInt(newRng(Int.MaxValue))
nonNegativeInt(newRng(0))


def nextD(i: Int) = {
  val (rnd, rng) = double(newRng(i))
  f"$rnd%1.20f"
}

nextD(0)
nextD(-1)
nextD(Int.MinValue)
nextD(Int.MaxValue)

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, rng1) = rng.nextInt
  val (d, rng2) = double(rng1)
  ((i, d), rng2)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val ((i,d), rng1) = intDouble(rng)
  ((d, i), rng1)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (d1, rng1) = double(rng)
  val (d2, rng2) = double(rng1)
  val (d3, rng3) = double(rng2)
  ((d1, d2, d3), rng3)
}

//def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//  if(count <= 0) (List(), rng)
//  else {
//    val (l: List[Int], r: RNG) = ints(count -1)
//    val (i,r2) = r.nextInt
//    (i :: l, r2)
//  }
//}

def int8(rng: RNG): (Int, RNG) = rng.nextInt

val int9: RNG => (Int, RNG) = rng => rng.nextInt

val int: (Int, RNG) = int8(newRng(0))
val int1: (Int, RNG) = int9(newRng(0))

val x = int9
val y = int9

x == y


def unit(i: Int): RNG => (Int, RNG) = rng => (i, rng)

def map(stateFunction: RNG => (Int, RNG))(map: Int => String): RNG => (String, RNG) =
  rng => {
    val (v, rng2) = rng.nextInt
    (map(v), rng2)
  }


val stateTransformer = map(unit(1))("Hello " + _)
val (v2, s2) = stateTransformer(newRng(5))
val (v3, s3) = stateTransformer(s2)
