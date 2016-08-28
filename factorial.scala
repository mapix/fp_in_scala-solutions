
def factorial(n: Int): Int = {

  @annotation.tailrec
  def go(n: Int, acc: Int) =
    if (n <= 0) acc
    else go(n - 1, acc * n)

  go(n, 1)
}
