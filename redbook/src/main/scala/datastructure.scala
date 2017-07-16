import annotation._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldLeft[A,B](as:List[A], z:B)(f:(B,A)=> B):B =
  as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t,f(z,h))(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x+y)

  def append2[A](l: List[A], r:List[A]):List[A] = {
    foldLeft(l, r)((x,y) => Cons(y,x))
  }

  def foldRight[A,B](as: List[A], z:B)(f:(A,B)=>B):B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs,z)(f))
    }

  def append[A](a1: List[A], a2:List[A]):List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h,append(t,a2))
    }
  }
  def sum2(ns: List[Int]) =
    foldRight(ns,0)((x,y)=>x+y)

  def product2(ns: List[Double]) =
    foldRight(ns,1.0)(_ * _)

  def length[A](as: List[A]):Int =
    foldRight(as,0)( (_,x) => x+1 )


  def tail[A](l:List[A]):List[A] = l match {
    case Cons(_,xs) => xs
    case Nil => sys.error("tail of empty list")
  }

  def setHead[A](h:A, l:List[A]):List[A] = l match {
    case Cons(_,xs) => Cons(h,xs)
    case Nil => sys.error("tail of empty list")
  }

  def drop[A](l:List[A], n:Int):List[A] = {
    if (n == 0) l
    else l match {
      case Cons(_,t) => drop(t,n-1)
      case Nil => sys.error("tail of empty list")
    }
  }
  def dropWhile[A](l:List[A])(f: A => Boolean):List[A] = {
    l match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }
  def init[A](l:List[A]):List[A] =
    l match {
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
}