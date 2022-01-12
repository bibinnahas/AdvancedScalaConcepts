package com.bbn.fp.restart

import scala.annotation.tailrec

object factAndPrime extends App {

  def factorial(n: Int): BigInt = {
    @tailrec
    def factHelper(x: Int, acc: BigInt): BigInt = {
      if (x == 1) acc
      else factHelper(x - 1, x * acc)
    }
    factHelper(n, 1)
  }

  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean = {
      if (t <= 1) true
      else n % t != 0 && isPrimeUntil(t - 1)
    }
    isPrimeUntil(n/2)
  }

  println(factorial(100))

}
