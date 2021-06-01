package com.bbn.fp.basic

object Options extends App {

  def unsafeMethod: Option[String] = None

  def safeMethod: Option[String] = Some("A valid result")

  val result: Option[String] = unsafeMethod.orElse(safeMethod)

  val someFunc: Option[Int] = Some(4)

  println(someFunc.flatMap(x => Option(x * 10)))
  println(result)

}
