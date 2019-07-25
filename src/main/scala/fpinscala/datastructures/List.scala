package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def exercise_3_1(): Int = {
    List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x // No match
      case Nil => 42 // No match
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3
      case Cons(h, t) => h + List.sum(t) // 15
      case _ => 101 // 101
    }
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => List(x)
    case Cons(_, xs) => Cons(x, xs)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  // this is a version of dropWhile where we don't have to specify the types in function f
  // see the main for an example on how to call it
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }

  // Exercise 3.6
  // This init function takes O(n-1)=O(n) time, n is the number of elements in the list
  // because it creates a new list with the n-1 first elements
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, y) => Cons(x, init(y))
  }

  // Exercise 3.7
  // Product with fold right will only stop when the end of the list is reached.
  // Function foldRight could have an additional parameter 'halt' to stop and return that value.


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.8
  def exercise_3_8(ns: List[Int]): List[Int] =
    List.foldRight(ns, Nil:List[Int])(Cons(_,_))

  // Exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,y) => y + 1)
  }

  // Exercise 3.10
  @scala.annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](l: List[A]): Int = {
    foldLeft(l, 0)((x,_) => x + 1)
  }

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
