package com.bbn.fp.basic

import java.lang

/**
 * Functional Programming concepts
 */
object WhatsFunction extends App {

  val adder = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }
  val adderSimplified: ((Int, Int) => Int) = (v1: Int, v2: Int) => v1 + v2

  val stringConcatenator: ((String, String) => String) = (s1: String, s2: String) => s1 + ' ' + s2

  def myFunc: Int => (Int => Int) = (v1: Int) => (v2: Int) => v1 + v2

  //  MOAR synctatic sugar
  val moarIncrementer: Int => Int = _ + 1

  //  LAMBDA
  println(adder(1, 2))
  println(adderSimplified(1, 2))
  println(stringConcatenator("bibin", "nahas"))
  //  Curried function
  println(myFunc(5)(4))
  //  MOAR
  println(moarIncrementer(7))

}
