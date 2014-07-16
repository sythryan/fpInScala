package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) 
  }

  def max(t: Tree[Int]): Int = {
    def innerMax(t: Tree[Int], currentMax: Int): Int = t match {
      case Leaf(x) => x max currentMax
      case Branch(left, right) => innerMax(left, currentMax) max innerMax(right, currentMax)
    }
    innerMax(t, 0)
  }

  def depth[A](t: Tree[A]): Int = {
    def innerDepth(t: Tree[A], currentDepth: Int): Int = t match {
      case Leaf(_) => currentDepth
      case Branch(left, right) => innerDepth(left, currentDepth + 1) max innerDepth(right, currentDepth + 1)
    }
    innerDepth(t, 0)
  }
  
  def map[A](t: Tree[A]) (f: A => A): Tree[A] = {
    def innerMap(t: Tree[A]): Tree[A] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(innerMap(left), innerMap(right))
    }
    innerMap(t)    
  }

    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
      def innerFoldLeft(l: List[A], acc: B): B = l match {
        case Cons(x, xs) => innerFoldLeft(xs, f(acc, x))
        case Nil => acc 
      }
      innerFoldLeft(l, z)
    }
}