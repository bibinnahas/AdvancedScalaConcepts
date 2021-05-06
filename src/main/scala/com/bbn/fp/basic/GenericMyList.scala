package com.bbn.fp.basic

abstract class GenericMyList[+A] {
  def head: A

  def tail: GenericMyList[A]

  def isEmpty: Boolean

  def add[B >: A](elem: B): GenericMyList[B]

  def printElements: String

  override def toString: String = "[" + printElements + "]"

}

object Empty extends GenericMyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: GenericMyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](elem: B): GenericMyList[B] = new Cons(elem, Empty)

  def printElements: String = ""
}

class Cons[+A](h: A, t: GenericMyList[A]) extends GenericMyList[A] {
  def head: A = h

  def tail: GenericMyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](elem: B): GenericMyList[B] = new Cons(elem, new Cons(h, t))

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  }
}

object ListTest extends App {

  val listOfIntegers = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings = new Cons("bibin", new Cons("nahas", new Cons("is awesome", Empty)))

  println(listOfIntegers.head)
  println(listOfStrings.head)
  println(listOfStrings.add("kunjan").head)
}

