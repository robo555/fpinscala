package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => Cons(h, tail)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) {
      l
    } else {
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)((x, y) => x * y)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((newList, newHead) => Cons(newHead, newList))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((acc, tail) => Cons(acc, tail))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())(append2)
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((head, acc) => Cons(head + 1, acc))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, List[String]())((head, acc) => Cons(head.toString, acc))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((head, acc) => Cons(f(head), acc))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((head, acc) => {
      if (f(head)) {
        Cons(head, acc)
      } else {
        acc
      }
    })
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((head, acc) => append2(f(head), acc))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((a) => if (f(a)) List(a) else Nil)
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
}
