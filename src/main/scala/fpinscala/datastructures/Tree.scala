package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26
  def maximum(root: Tree[Int]): Int = root match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
   }

  // Exercise 3.27
  def depth[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A](root: Tree[A])(f: A => A): Tree[A] = root match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29
  // Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  // Reimplement them in terms of this more general function.
  // Can you draw an analogy between this fold function and the left and right folds for List?
  // def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  //    case Nil => z
  //    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  //  }
  def fold[A, B](root: Tree[A])(z: Leaf[A] => B)(f: (B, B) => B): B = root match {
    case v: Leaf[A] => z(v)
    case b: Branch[A] => f(fold(b.left)(z)(f), fold(b.right)(z)(f))
  }

  def sizeWithFold[A](root: Tree[A]): Int =
    fold(root)(_ => 1)((a, b) => 1 + a + b)

  def maximumWithFold(root: Tree[Int]): Int =
    fold(root)(x => x.value)((a: Int, b: Int) => a max b)

  def depthWithFold[A](root: Tree[A]): Int =
    fold(root)(_ => 1)((a, b) => 1 + (a max b))

  def mapWithFold[A](root: Tree[A])(f: A => A): Tree[A] =
    fold(root)(x => Leaf(f(x.value)): Tree[A])((a, b) => Branch(a, b))
}