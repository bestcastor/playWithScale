package Modules

/**
  * Created by nyu on 8/27/16.
  *  x*x*x -2*x*x + 5
  *
  *  Map(0 -> 5, 1 -> -2, 3 -> 1)
  */
class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
      exp -> (coeff + terms(exp))
  }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff+"x^"+exp) mkString "+"

  //Another way to implement '+' function with foldRight
  def add (other: Poly) = new Poly( (other.terms foldLeft terms)(addterm))
  def addterm(terms:Map[Int, Double], term : (Int, Double)) :Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff+terms(exp)))
  }
}