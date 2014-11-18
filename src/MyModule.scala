object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) {
        acc
      } else {
        go(n - 1, n * acc)
      }
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(acc: Int, m: Int, i: Int): Int = {
      if (i == n) acc
      else loop(m, m + acc, i + 1)
    }
    loop(0, 1, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    }
    loop(1)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))

    println(factorial(5).toString)

    println(fib(5))

    if (isSorted[Int](Array(1, 2, 3), (x, y) => x <= y)) {
      println("Trié")
    } else {
      println("Non trié")
    }
  }
}
