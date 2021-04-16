package com.bbn.fp.basic

object HOF extends App {
  def nTimes(f: Int => Int, n: Int, x: Int): Int = {
    if (n <= 0) x
    else nTimes(f, n - 1, f(x))
  }

  def betterNTimes (f: Int => Int, n: Int): (Int => Int) = {
    if (n <= 0) (x: Int) => x
    else (x: Int) => betterNTimes(f, n - 1)(f(x))
  }

  def inc: Int => Int = _ + 1

  def curriedFormatter(x: String)(y: Double): String = x.format(y)
  val standardFormat: Double => String = curriedFormatter("%4.2f")

  println(nTimes(inc, 10, 5))
  println(betterNTimes(inc, 10)(1))
  println(curriedFormatter("%4.4f")(Math.PI))
  println(standardFormat(Math.PI))
}
