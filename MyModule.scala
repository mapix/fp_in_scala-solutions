
object MyModule {

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, acc * n)
    go(n, 1)
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factorial value of %d is %d"
    msg.format(x, factorial(x))
  }

  def formatResult(name: String, x: Int, f: Int => Int) = {
    val msg = "The %s value of %d is %d"
    msg.format(name, x, factorial(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(10))
  }
}
