package Modules

/**
  * Created by nyu on 8/11/16.
  */
trait List[+T] {
  def isEmpty:Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

object Nil extends  List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail : List[T]) extends List[T] {
  def isEmpty = false
}

object List {
  val h:List[String] = Nil

  //List(1,2) = List.apply(1,2)
  def apply[T](x1: T, x2 :T) : List[T] = new Cons(x1, new Cons(x2,  Nil))
  //List() = List.apply()
  def apply[T] =  Nil

//  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
//    case List() => List(x)
//    case y :: ys => ???
//  }
}

