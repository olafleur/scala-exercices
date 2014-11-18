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

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  def curry[A, B, C](f:(A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
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

    val func = partial1[Int, Int, String](3, (n, m) => (n + m).toString)

    println(func(4))

    val curryfied = curry[Int, Int, String]((n, m) => (n + m).toString)

    println(curryfied(3)(4))

    println(uncurry(curryfied)(3, 4))

    val liste = List.setHead(List(1, 2, 3), 4)

    println(liste)

    val liste2 = List(1, 2, 3, 4)

    println(List.drop(liste2, 2))

    val liste3 = List(2, 4, 6, 8, 10, 13, 7, 8)

    println(List.dropWhile(liste3)(_ % 2 == 0))

    val liste4 = List(1, 2, 3, 4, 5, 6)

    println(List.init(liste4))
  }
}
