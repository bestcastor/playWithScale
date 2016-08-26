package Modules

/**
  * Created by nyu on 8/11/16.
  */
class Rational(x: Int, y: Int){
  //precondition check
  require(y!= 0, "Denominator cannot be zero!")

  //constructor
  def this(x : Int) = this(x, 1)

  //Try to simpify the the rational.
  private def gcd(a : Int, b : Int) : Int=
    if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  val numer = x/g
  val denom = y/g

  //-x
  def unary_- = new Rational(-numer, denom)

  //x+y
  def +(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )
  //x-y
  def - (that: Rational) = this + -that


  //x*y
  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)
  //x<y?
  def <(that: Rational) =
    numer * that.denom < denom * that.numer
  //x>y? x : y
  def max(that: Rational) =
    if (this < that) that else this

  override def toString = numer + "/" + denom
}