object exercise2 {
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def innerIsSorted(anArray: Array[A]): Boolean = anArray.length match {
      case 1 => true
      case _ => if (gt(anArray(0), anArray(1))) innerIsSorted(anArray.tail) else false
    }
    innerIsSorted(as)
  }

  def ascending(x: Int, y: Int)= (x < y)

  def main(args: Array[String]): Unit = {
    println("is Array(1, 2, 3) in ascending order: " + isSorted(Array(1,2,3), ascending))
    println("is Array(3, 2, 1) in ascending order: " + isSorted(Array(3,2,1), ascending))
  }
  
}