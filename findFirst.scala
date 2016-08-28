
object X {
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }
  
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = 
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    loop(0)
  }

  val lessThan = new Function2[Int, Int, Boolean] {
    def apply(a: Int, b: Int) = a < b
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = 
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def andThen[A, B, C](g: A => B, f: B => C): A => C =
    a => f(g(a))
}
