sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if !f(a) => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
//  def failingFn(i: Int): Int = {
//    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
//    val y: Int = throw new Exception("fail!")
//    try {
//      val x = 42 + 5
//      x + y
//    }
//    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
//    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
//    catch { case e: Exception => 43 }
//  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))
}