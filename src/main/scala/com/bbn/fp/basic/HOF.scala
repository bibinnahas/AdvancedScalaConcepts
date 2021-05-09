package com.bbn.fp.basic

object HOF {
  def nTimesApplier(f: Int => Int, n: Int): Int => Int = {
    if (n <= 0) (x: Int) => x
    else (x: Int) => nTimesApplier(f, n - 1)(f(x))
  }

  def plus1(x: Int): Int = x + 1

  def toCurry(f: (Int, Int) => Int): (Int => Int => Int) = x => y => f(x, y)

  def fromCurry(f: (Int => Int => Int)): ((Int, Int) => Int) = (x, y) => f(x)(y) //need to understand this

}

object Test extends App {
  println(HOF.nTimesApplier(HOF.plus1, 10)(10))
}
