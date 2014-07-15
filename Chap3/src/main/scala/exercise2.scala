package fpinscala.datastructures

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
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(x, xs) => xs
    case _ => Nil
  }

  def setHead[A](head: A, as: List[A]): List[A] = as match {
    case Cons(x, xs) => Cons(head, xs)
    case _ => Cons(head, Nil)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(x, _) if (n < 1) => l 
    case Cons(x, xs) => drop(xs, n-1)
    case _ => Nil 
  }
  
  def dropWhile[A](l: List[A])( f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }

  def foldRight[A,B](as: List[A], z: B) (f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x,y) => y + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def innerFoldLeft(l: List[A], acc: B): B = l match {
      case Cons(x, xs) => innerFoldLeft(xs, f(acc, x))
      case Nil => acc 
    }
    innerFoldLeft(l, z)
  }
  
}