sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => sys.error("Tail doesn't exist")
      case Cons(_,t) => t
    }
  }
  def setHead[A](li: List[A], a: A): List[A] = Cons(a, li)
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 1) tail(l) else drop(tail(l), n-1)
  }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,_) if (f(h)) => dropWhile(tail(l), f)
    case _ => l
  }
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Its broke")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
  }
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def product2(ds: List[Double]): Double = {
    foldRight(ds, 1.0)(_ * _)
  }
}





//exercise 1
val e1 = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}
assert(e1 == 3)
//exercise 2
var e2_1 = List.tail(List(1,2,3))
assert(e2_1 == List(2,3))
var e2_2 = List.tail(List(1))
assert(e2_2 == Nil)
//exercise 3
var e3 = List.setHead(List(1,2,3), 100)
assert(e3 == List(100,1,2,3))
//exercise 4
var e4 = List.drop(List(1,2,3,4,5), 2)
assert(e4 == List(3,4,5))
//exercise 5
var e5 = List.dropWhile(List(1,2,3,4,5), (i: Int) => i < 4)
assert(e5 == List(4,5))
//exercise 6
var e6 = List.init(List(1,2,3,4))
assert(e6 == List(1,2,3))
//exercise 7
var e7 = List.product2(List(1.0,1.0,4.0))
assert(e7 == 4.0)






