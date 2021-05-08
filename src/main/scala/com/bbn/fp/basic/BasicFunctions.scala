package com.bbn.fp.basic

object BasicFunctions extends App {
  val sampleFunc1 = new Function1[Int, Int] {
    override def apply(elem: Int): Int = elem * 2
  }

  val sampleFunc2: (Int => Int) = new Function1[Int, Int] {
    override def apply(elem: Int): Int = elem * 2
  }

  val sampleFunc3: (Int => Int) = (elem: Int) => elem * 2

  val anotherFunc1 = new Function2[Int, Int, Int] {
    def apply(v1: Int, v2: Int): Int = v1 + v2

    val anotherFunc2: ((Int, Int) => Int) = (v1: Int, v2: Int) => v1 + v2
  }

  val concatenator: ((String, String) => String) = (s1: String, s2: String) => s1 + s2

  println(concatenator("Bibin", " Nahas"))
}

