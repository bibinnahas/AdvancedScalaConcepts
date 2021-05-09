package com.bbn.fp.basic

object HOF extends App {
  def nTimesApplier(f: Int => Int, n: Int): Int => Int = {
    if (n <= 0) (x: Int) => x
    else (x: Int) => nTimesApplier(f, n - 1)(f(x))
  }

  def plus1(x: Int): Int = x + 1

  println(nTimesApplier(plus1, 10)(10))
}
