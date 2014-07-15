object exercise5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = 
    f(g(_))
}