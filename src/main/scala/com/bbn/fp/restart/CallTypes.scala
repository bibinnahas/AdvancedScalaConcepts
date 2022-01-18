package com.bbn.fp.restart

object CallTypes {
  def callByName(time: Long): Unit = {
    println("by name" + time)
    println("by name" + time)
  }

  def callByValue(time: => Long): Unit = {
    println("by value" + time)
    println("by value" + time)
  }

  def main(args: Array[String]): Unit = {
    callByName(System.nanoTime())
//    by name49006755812459
//    by name49006755812459

    callByValue(System.nanoTime())
//    by value49006918994125
//    by value49006919054500
  }
}
