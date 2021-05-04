package com.bbn.fp.basic

import scala.annotation.tailrec

object Functions {

  /**
   * A simple function to concatenate string to the number of times
   * using tail recurssion
   *
   * @param word
   * @param times
   * @return words * times
   */
  def stringConcatenator(word: String, times: Int = 5): String = {
    @tailrec
    def helper(word: String, times: Int, accu: String): String = {
      if (times == 0) accu
      else helper(accu, times - 1, word + accu)
    }

    helper(word, times, "")
  }

  /**
   * Simple function to find if a number is prime or not
   *
   * @param n
   * @return
   */
  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeUntil(t: Int): Boolean = {
      if (t <= 1) true
      else (n % t) != 0 && isPrimeUntil(t - 1)
    }

    isPrimeUntil(n / 2)
  }

  /**
   * Difference between call by name and call by value
   *
   * @param x
   */
  def callByName(x: => Long) = {
    println(s"by name: $x")
    println(s"by name: $x")
  }

  def callByValue(x: Long) = {
    println(s"by value: $x")
    println(s"by value: $x")
  }

  def main(args: Array[String]): Unit = {
    println(stringConcatenator("Hello ", 7))
    println(isPrime(7))
    //    println(callByName(System.nanoTime()))
    //    println(callByValue(System.nanoTime()))
  }

} 
