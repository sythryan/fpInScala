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
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
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

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_+_)

  def productLeft(ns: List[Int]) =
    foldLeft(ns, 1.0)(_*_)

  def lengthLeft[A](l: List[A]): Int = 
    foldLeft(l, 0)((x,y) => x + 1)
  
  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil: List[A])((x,y) => Cons(y, x))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def append[A](main: List[A], toAppend: List[A]): List[A] =
    foldRightViaFoldLeft(main, toAppend)(Cons(_,_))

  def concatenate[A](listOfLists: List[List[A]]): List[A] = {
    def innerConcatenate(remainingLists: List[List[A]], acc: List[A]): List[A] = remainingLists match {
      case Cons(h, t) => innerConcatenate(t, append(acc, h))
      case _ => acc
    }
    innerConcatenate(listOfLists, List(): List[A])
  }

  def addOneToAll(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int])((x,y) => Cons(x + 1, y))

  def doubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String])((x,y) => Cons(x.toString, y))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((x,y) => Cons(f(x),y))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])((x,y) => if (f(x)) Cons(x,y) else y)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    def flatten(lystOfLysts: List[List[B]]): List[B] =
      foldRightViaFoldLeft(lystOfLysts, Nil: List[B])((x,y) => append(x, y))

    flatten(foldRightViaFoldLeft(l, Nil: List[List[B]])((x,y) => Cons(f(x),y)))
  }

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def mergeAdd(lystOne: List[Int], lystTwo: List[Int]): List[Int] = (lystOne, lystTwo) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, mergeAdd(xs, ys))
  }

  def merge[A](lystOne: List[A], lystTwo: List[A], f: (A,A) => A ): List[A] =(lystOne, lystTwo) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), merge(xs, ys, f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    //check the sequence on each elem as a starting elem untill fully true
    def checkSequence(a: List[A], s: List[A]): Boolean = (a, s) match {
      case (Nil, Nil) => true
      case (Nil, _)   => false
      case (_, Nil)   => true
      case (Cons(x, xs), Cons(y, ys)) if (x == y) => checkSequence(xs, ys)
      case _ => false 
    }

    l match {
      case Cons(x, xs) if checkSequence(l, sub) => true
      case Cons(x, xs) => hasSubsequence(xs, sub)
      case _ => false
    }
  }
    

    
  
  
}