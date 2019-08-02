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
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  // Exercise 3.5
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  // this is a version of dropWhile where we don't have to specify the types in function f
  // see the main for an example on how to call it
  @scala.annotation.tailrec
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
    foldRight(ns, Nil:List[Int])(Cons(_,_))

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

  // Exercise 3.13
  //def foldLeftWithFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    // case Nil => z
    // case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //}

  // Exercise 3.14
  def append[A](l: List[A], x: A): List[A] =
    foldRight(l, List(x))((a, b) => Cons(a, b))

  // Exercise 3.15
  def concatenateLists[B](l1: List[B], l2: List[B]): List[B] = {
    foldRight(l1, l2)((a, b) => Cons(a, b))
  }

  def concatenateListsOfLists[A](ll: List[List[A]]): List[A] = {
    foldLeft(ll, Nil:List[A])(concatenateLists)
  }

  // Exercise 3.16
  def incrementList(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((a, b) => Cons(a+1, b))
  }

  // Exercise 3.17
  def applyToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))
  }

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((a, b) => concatenateLists(f(a), b))
  }

  // Exercise 3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(i => if (f(i)) List(i) else List())
  }

  // Exercise 3.22
  def sumLists(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => Nil
    case Cons(x, xs) => l2 match {
      case Nil => Nil
      case Cons(y, ys) => Cons(x + y, sumLists(xs, ys))
    }
  }

  // Exercise 3.23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = l1 match {
    case Nil => Nil
    case Cons(x, xs) => l2 match {
      case Nil => Nil
      case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  // Exercise 3.24
  // Missing

  def main(args: Array[String]): Unit = {
    println(3.0.toString)
  }

}
