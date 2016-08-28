package Modules


import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Modules.Poly

/**
  * Created by nyu on 8/27/16.
  */
@RunWith(classOf[JUnitRunner])
class PolynomialsSuite extends FunSuite{
  trait TestBase {
    val p1 = new Poly(1->2.0, 5 -> 6, 3 -> -2.0)
    val p2 = new Poly(0->3.0, 5-> 5.0, 4 -> 4.0)
  }

  test("test +()") {
    new TestBase {
      val p12 = p1 + p2
      println(p12)
      val s = p12.terms get 5 match {case Some(x) => x}

      assert(s == 11)
    }
  }

  test("test add()"){
    new TestBase {
      val p12 = p1 add p2
      println(p12)
      val s = p12.terms get 5 match {case Some(x) => x}

      assert(s == 11)
    }
  }

}
