package com.bbn.fp.basic

abstract class MyList {
  def head: Int

  def tail: MyList

  def isEmpty: Boolean

  def add(elem: Int): MyList

  def printElements: String

  override def toString: String = "[" + printElements + "]"

}

object Empty extends MyList {
  def head: Int = throw new NoSuchElementException

  def tail: MyList = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add(elem: Int): MyList = new Cons(elem, Empty)

  def printElements: String = ""
}

class Cons(h: Int, t: MyList) extends MyList {
  def head: Int = h

  def tail: MyList = t

  def isEmpty: Boolean = false

  def add(elem: Int): MyList = new Cons(elem, new Cons(h, t))

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  }
}

object ListTest extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(list.head)
  println(list.tail.head)
  println(list.isEmpty)
  println(list.add(4).head)
  println(list.add(4).toString)
}

