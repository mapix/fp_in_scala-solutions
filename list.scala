
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, ts) => ts
    }

  def setHead[A](xs: List[A], h: A): List[A] = Cons(h, xs)

  def drop[A](xs: List[A], n: Int): List[A] = xs match {
      case Nil => Nil
      case Cons(_, t) if n > 0  => drop(t, n - 1)
      case _ => xs
    }

  def dropWhile[A](xs: List[A], p: A => Boolean): List[A] = xs match {
      case Cons(head, tail) if p(head) => dropWhile(tail, p)
      case _ => xs
    }

  def dropWhile2[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
      case Cons(head, tail) if p(head) => dropWhile2(tail)(p)
      case _ => xs
    }

  def append[A](as: List[A], bs: List[A]): List[A] = as match {
      case Nil => bs 
      case Cons(x, xs) => Cons(x, append(xs, bs))
    }

  def init[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, tail) => Cons(h, init(tail))
    }

  def foldRight[A, B](xs: List[A], acc: B)(f: (A, =>B) => B): B = xs match {
      case Nil => acc
      case Cons(head, tail) => {
          f(head, foldRight(tail, acc)(f))
        }
   }

  def foldLeft[A, B](xs: List[A], acc: B)(f: (A, B) => B): B = xs match {
      case Nil => acc
      case Cons(head, tail) => foldLeft(tail, f(head, acc))(f)
    }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_+_)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)((a, b) => if (a == 0.0) 0.0 else a * b)

  def length2[A](xs: List[A]): Int = 
    foldRight(xs, 0)((_, b) => b + 1)

  def sum3(ints: List[Int]): Int =
    foldLeft(ints, 0)(_+_)

  def product3(ds: List[Double]): Double =
    foldLeft(ds, 1.0)((a, b) => if (a == 0.0) 0.0 else a * b)

  def length3[A](xs: List[A]): Int = 
    foldLeft(xs, 0)((_, b) => b + 1)

  def reversed[A](xs: List[A]): List[A] = 
    foldLeft(xs, Nil:List[A])(Cons(_, _))

  def foldRight2[A, B](xs: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reversed(xs), acc)(f)

  def append2[A](as: List[A], bs: List[A]): List[A] = 
    foldRight(as, bs)(Cons(_, _))

  def flatten[A](xs: List[List[A]]): List[A] = 
    foldRight(xs, Nil:List[A])(append2(_, _))

  def increased(xs: List[Int]): List[Int] = 
    foldRight2(xs, Nil:List[Int])((x: Int, acc: List[Int]) => Cons[Int](x + 1, acc))

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    foldRight2(xs, Nil:List[B])((x: A, acc: List[B]) => Cons(f(x), acc))

  def filter[A](xs: List[A])(f: A => Boolean): List[A] = 
    foldRight2(xs, Nil:List[A])((x: A, acc: List[A]) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = 
    flatten(map(xs)(f))

  def filter2[A](xs: List[A])(f: A => Boolean): List[A] = 
    flatMap(xs)(x => if (f(x)) List(x) else Nil)

  def zipInt(as: List[Int], bs: List[Int]):List[Int] = {
    def _zip(as: List[Int], bs: List[Int], acc: List[Int]):List[Int] = (as, bs) match {
      case (Cons(ax, axt), Cons(bx, bxt)) => _zip(axt, bxt, Cons(ax+bx, acc))
      case _ => acc
    }
    reversed(_zip(as, bs, Nil:List[Int]))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C):List[C] = {
    def _zip(as: List[A], bs: List[B], acc: List[C]):List[C] = (as, bs) match {
      case (Cons(ax, axt), Cons(bx, bxt)) => _zip(axt, bxt, Cons(f(ax, bx), acc))
      case _ => acc
    }
    reversed(_zip(as, bs, Nil:List[C]))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(xs: List[A], sxs: List[A]): Boolean = (xs, sxs) match {
      case (_, Nil) => true
      case (Cons(x, tail), Cons(y, stail)) if (x == y && hasSubsequence(tail, stail)) => true
      case (Cons(_, tail), stail) => hasSubsequence(tail, stail)
    }
    go(sup, sub)
  }
}
