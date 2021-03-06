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

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil //Pourrait être une erreur
    case Cons(_, Nil) => Nil //Pas nécessaire
    case Cons(_, as) => as
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, as) => Cons(h, as)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(List.tail(l), n - 1) //Aurait pu utiliser Case Cons(_, t) à la place
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) =>
      if (f(a)) dropWhile(as)(f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => 1 + y)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A, B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}