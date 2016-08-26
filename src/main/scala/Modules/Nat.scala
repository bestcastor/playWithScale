package Modules

/**
  * Created by nyu on 8/11/16.
  */

//Peano
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("0.predecessor")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = {
    if(that.isZero) Zero
    else throw new Error("negative integer is not supported")
  }

}
class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
//    if (that.isZero) this
//    else successor + that.predecessor
//  }

  def - (that: Nat): Nat = {
    if (that.isZero) this
    else this.predecessor - that.predecessor
  }
}
