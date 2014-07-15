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
    case Cons(0.0, _) => 0.0
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
  
  
}