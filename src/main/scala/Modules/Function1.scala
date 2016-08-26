package Modules

/**
  * Created by nyu on 8/11/16.
  */

//Function is an object as well. e.g.
trait Function1[A, B] {
  def apply(x: A) :B
}

class AnonFun extends  Function1[Int, Int] {
  def apply(x: Int) = x * x
}

/*
val f = (x:Int) => x * x
f(7)

------ equals to:

val f = new Functions1[Int, Int] {
  def apply(x: Int) = x * x
}

f.apply(7)
 */