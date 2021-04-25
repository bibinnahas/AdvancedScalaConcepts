package com.bbn.fp.basic

import scala.annotation.tailrec

object Functions {

  /**
   * A simple function to concatenate string to the number of times
   * using tail recurssion
   * @param word
   * @param times
   * @return words * times
   */
  def stringConcatenator(word: String, times: Int): String = {
    @tailrec
    def helper(word: String, times: Int, accu: String): String = {
      if (times == 0) accu
      else helper(accu, times - 1, word + accu)
    }
    helper(word, times, "")
  }

  def main(args: Array[String]): Unit = {
    println(stringConcatenator("Hello ", 5))
  }

}
