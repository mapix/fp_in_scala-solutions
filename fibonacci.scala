
object T extends App {

  def fibonacci(n: Int): Int = {
  
    @annotation.tailrec
    def go(n: Int, i: Int, acc1: Int, acc2: Int): Int = 
      if (n == 0) acc2
      else if (n == 1) acc1
      else if (n == i+1) acc1 + acc2
      else go(n, i + 1, acc1 + acc2, acc1)
  
    go(n, 1, 1, 0)
  }

  override def main(args: Array[String]): Unit = {
    println(0, fibonacci(0))
    println(1, fibonacci(1))
    println(2, fibonacci(2))
    println(3, fibonacci(3))
    println(4, fibonacci(4))
    println(5, fibonacci(5))
    println(6, fibonacci(6))
  }
}
