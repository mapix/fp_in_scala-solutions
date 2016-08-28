
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maxinum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maxinum(left) max maxinum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => (depth(left)+1) max (depth(right)+1)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(f2: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(left, right) => f2(fold(left)(f)(f2), fold(right)(f)(f2))
  }

  def size2[A](tree: Tree[A]): Int =
    Tree.fold(tree)(_ => 1)(1+_+_)

  def maxinum2(tree: Tree[Int]): Int =
    Tree.fold(tree)(x => x)(_ max _)

  def depth2[A](tree: Tree[A]): Int =
    Tree.fold(tree)(_ => 1)(_ + 1 max _ + 1)

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    Tree.fold(tree)(x => Leaf(f(x)).asInstanceOf[Tree[B]])((l, r) => Branch(l, r))
}
