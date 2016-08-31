
def rollDie: Int = {
  var rng = new scala.util.Random
  rng.nextInt(6)
}

def rollDie2(rng: scala.util.Random): Int = rng.nextInt(6)

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

def randomPair(rng: RNG): (Int, Int) = {
  var (i1, _) = rng.nextInt
  var (i2, _) = rng.nextInt
  (i1, i2)
}

def randomPair2(rng: RNG): ((Int, Int), RNG) = {
  var (i1, r1) = rng.nextInt
  var (i2, r2) = r1.nextInt
  (i1, i2) ->  r2
}

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  var (i, r) = rng.nextInt
  (if (i >= 0) i else if (i == Int.MinValue) Int.MaxValue else -i, r)
}

def double(rng: RNG): (Double, RNG) = {
  var (i, r) = rng.nextInt
  (i.toDouble / Int.MaxValue, r)
}


def intDouble(rng: RNG): ((Int,Double), RNG) = {
  var (i, r1) = rng.nextInt
  var (d, r2) = double(r1)
  (i, d) -> r2
}


def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  var (d, r1) = double(rng)
  var (i, r2) = r1.nextInt
  (d, i) -> r2
}

def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  var (d1, r1) = double(rng)
  var (d2, r2) = double(r1)
  var (d3, r3) = double(r2)
  (d1, d2, d3) -> r3
}


def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  if (count == 0) {
      var (int, r) = rng.nextInt
      List(int) -> r
  } else {
      var (intl, r) = ints(count-1)(rng)
      var (int, r2) = r.nextInt
      (int::intl, r2)
  } 
}

type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] = rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
  rng => {
    var (r, sb) = s(rng)
    (f(r), sb)
  } 
}

def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - i % 2)

def double2(rng: RNG): (Double, RNG) = 
  map(_.nextInt)(_.toDouble / Int.MaxValue)(rng)


def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  rng => {
    val (ar, nra) = ra(rng)
    val (br, nrb) = rb(nra)
    (f(ar, br), nrb)
  }
}

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_, _))


val randIntDouble: Rand[(Int, Double)] =
  both(_.nextInt, double2)


val randDoubleInt: Rand[(Double, Int)] =
  both(double2, _.nextInt)

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  rng => {
    fs.foldRight((Nil:List[A], rng))({
      case (randa, (lista, rng2)) =>
        var (a, rng3) = randa(rng2)
        (a::lista, rng3)
    })
  }
}


def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
  sequence(List.fill(count)((x: RNG) => x.nextInt))(rng)
