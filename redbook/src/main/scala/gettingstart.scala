/**
  * Created by wonderfulboyx on 17/06/18.
  */
object main {
  // フィボナッチ数を取得する再帰関数
  def fib(n: Int):Int = {
    if(n == 1) 0
    else if(n == 2) 1
    else fib(n-1)+fib(n-2)
  }
  // check whether array is sorted
  def isSorted[A](as: Array[A], ordered: (A,A)=> Boolean):Boolean = {
    @annotation.tailrec
    def loop(n: Int):Boolean =
      if (n >= as.length-2) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }
  def curry[A,B,C](f:(A, B) => C):A => (B=>C) =
    a => f(a, _)
  def uncurry[A,B,C](f:A=>B=>C):(A,B)=>C =
    (a, b) => f(a)(b)
  def compose[A,B,C](f:B=>C, g:A=>B):A=>C =
    a => f(g(a))
  def main(args: Array[String]): Unit = {
    println(fib(6))
  }
}
