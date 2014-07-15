object nthFibonacciNum {
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 0
    case 2 => 1
    case 3 => 1
    case _ => fib(n-1) + fib(n-2) 
  }
  def main(args: Array[String]): Unit = {
    println("The 7th Fibonacci number is " + fib(7))
  }
}