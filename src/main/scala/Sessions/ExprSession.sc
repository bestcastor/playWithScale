trait Expr {
  def evl : Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.evl + e2.evl
    case Prod(e1, e2) => e1.evl * e2.evl
  }

  def show : String = this match {
    case Number(n) => "" + n
    case Sum(e1, e2) => "(" + e1.show + " + " + e2.show + ")"
    case Prod(e1,e2) => e1.show + " * " + e2.show

  }

}

case class Number(n : Int) extends Expr
case class Sum(e1 : Expr, e2 : Expr) extends Expr
case class Prod(e1: Expr, e2 : Expr) extends Expr

//object Number {
//  def apply(n: Int) = new Number(n)
//}
//
//object Sum {
//  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
//}


val sum = Sum(Number(1), Number(2))
sum.evl
sum.show

val prod = Prod(sum, Number(3))
prod.show
prod.evl

val complex = Sum(prod, Prod(Number(10), Number(2)))

complex.show
complex.evl



