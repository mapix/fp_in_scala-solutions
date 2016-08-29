

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, tail) => h() :: tail().toList
  } 

  def take(n: Int): Stream[A] = this match {
    case Cons(hd, tl) if n > 0 => cons(hd(), tl().take(n-1))
    case _ => Empty
  }
 
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n == 0 => this
    case Cons(_, tl) => tl().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(hd, tl) => p(hd()) || tl().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption2: Option[A] =
    foldRight(None:Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty:Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def append[B >: A](sb: Stream[B]): Stream[B] =
    foldRight(sb)((a, b) => cons(a, b))

  def flatMap[B](f: A => Iterable[B]): Stream[B] =
    foldRight(Empty:Stream[B])((a, b) => f(a).foldRight(b)((x, y) => cons(x, y)))

  def map2[B](f: A => B): Stream[B] =
    unfold(this)({
      case Cons(hd, tl) => Some(f(hd()), tl())
      case _ => None
    })

  def take2(n: Int): Stream[A] =
    unfold((this, n))({
      case (Cons(hd, tl), n) if n > 0 =>  Some(hd(), (tl(), n - 1))
      case _ => None
    })

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this)({
      case Cons(hd, tl) if p(hd()) =>  Some(hd(), tl())
      case _ => None
    })

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C):Stream[C] =
    unfold((this, bs))({
      case (Cons(hd, tl), Cons(hd2, tl2)) =>  Some(f(hd(), hd2()), (tl(), tl2()))
      case _ => None
    })

  def zipAll[B](bs: Stream[B]): Stream[(Option[A],Option[B])] = 
    unfold((this, bs))({
      case (Cons(hd, tl), Cons(hd2, tl2)) =>  Some((Some(hd()), Some(hd2())), (tl(), tl2()))
      case (Cons(hd, tl), Empty) =>  Some((Some(hd()), None), (tl(), Empty))
      case (Empty, Cons(hd2, tl2)) =>  Some((None, Some(hd2())), (Empty, tl2()))
      case _ => None
    })

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (x, y) => x == y
    }

  def tails: Stream[Stream[A]] = 
    unfold(this)({
      case r@Cons(hd, tl) => Some(r -> tl())
      case _ => None
    }) append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => hd, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, Stream.constant(a))

  def from(n: =>Int): Stream[Int] =
    Stream.cons(n, Stream.from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => Empty
  }

  def constant2[A](a: A): Stream[A] =
    unfold(a)(a => Some(a, a))

  def from2(n: =>Int): Stream[Int] =
    unfold(n)(n => Some(n + 1, n + 1))

}


def fibs(): Stream[Int] = {
  def _fib(n: Int): Int = 
    if (n == 0) 0
    else if (n == 1) 1
    else _fib(n - 2) + _fib(n - 1)
  Stream.from(0).map(_fib)
}

def fibs2(): Stream[Int] = {
  Stream.cons(0, Stream.cons(1, Stream.unfold((0, 1))(s => Some(s._1 + s._2, (s._2, s._1 + s._2)))))
}
