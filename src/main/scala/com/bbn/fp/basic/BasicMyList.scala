package com.bbn.fp.basic

abstract class BasicMyList {
  def head: Int

  def tail: BasicMyList

  def isEmpty: Boolean

  def add(elem: Int): BasicMyList

  def printElements: String

  override def toString: String = "[" + printElements + "]"

}

object BasicListEmpty extends BasicMyList {
  def head: Int = throw new NoSuchElementException

  def tail: BasicMyList = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add(elem: Int): BasicMyList = new BasicListCons(elem, BasicListEmpty)

  def printElements: String = ""
}

class BasicListCons(h: Int, t: BasicMyList) extends BasicMyList {
  def head: Int = h

  def tail: BasicMyList = t

  def isEmpty: Boolean = false

  def add(elem: Int): BasicMyList = new BasicListCons(elem, new BasicListCons(h, t))

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  }
}

object BasicListTest extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(list.head)
  println(list.tail.head)
  println(list.isEmpty)
  println(list.add(4).head)
  println(list.add(4).toString)
}

