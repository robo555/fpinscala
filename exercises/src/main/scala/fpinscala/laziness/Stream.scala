package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {
  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  @tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else empty)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h).append(t))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if n > 1 => Some(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Some((h(), empty))
      case _ => None
    }
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t().takeWhile(f))
      case _ => None
    }
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    zipWithAll(s2)((_, _))
  }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Empty, Cons(h2, t2)) => Some(f(Option.empty[A], Some(h2())) -> (empty[A], t2()))
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), Option.empty[B]) -> (t1(), empty[B]))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
      case _ => None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.isDefined).forAll {
      case ((a, b)) => a == b
    }
  }

  def tails: Stream[Stream[A]] = {
    ???
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, loop(n2, n1 + n2))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case _ => empty
    }
  }

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(x => Some((1, 1)))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(x => Some((a, a)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(x => Some((x, x + 1)))
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1))(x => x match {
      case (n1, n2) => Some((n1, (n2, n1 + n2)))
    })
  }
}