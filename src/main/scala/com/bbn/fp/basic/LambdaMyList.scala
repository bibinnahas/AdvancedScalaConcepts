package com.bbn.fp.basic

abstract class LambdaMyList[+A] {
  def head: A

  def tail: LambdaMyList[A]

  def isEmpty: Boolean

  def add[B >: A](elem: B): LambdaMyList[B]

  def printElements: String

  // polumorphic call
  override def toString: String = "[" + printElements + "]"

  // HOF's - functions that either receive or return functions
  def map[B](transformer: (A => B)): LambdaMyList[B]

  def flatMap[B](transformer: (A => LambdaMyList[B])): LambdaMyList[B]

  def filter(predicate: (A => Boolean)): LambdaMyList[A]

  def ++[B >: A](lst: LambdaMyList[B]): LambdaMyList[B]

}

case object LambdaEmpty extends LambdaMyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: LambdaMyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](elem: B): LambdaMyList[B] = new LambdaCons(elem, LambdaEmpty)

  def printElements: String = ""

  def map[B](transformer: (Nothing => B)): LambdaMyList[B] = LambdaEmpty

  def flatMap[B](transformer: (Nothing => LambdaMyList[B])): LambdaMyList[B] = LambdaEmpty

  def filter(predicate: (Nothing => Boolean)): LambdaMyList[Nothing] = LambdaEmpty

  def ++[B >: Nothing](lst: LambdaMyList[B]): LambdaMyList[B] = lst
}

case class LambdaCons[+A](h: A, t: LambdaMyList[A]) extends LambdaMyList[A] {
  def head: A = h

  def tail: LambdaMyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](elem: B): LambdaMyList[B] = new LambdaCons(elem, this)

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  }

  def map[B](transformer: (A => B)): LambdaMyList[B] =
    new LambdaCons(transformer(h), t.map(transformer))

  def flatMap[B](transformer: (A => LambdaMyList[B])): LambdaMyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  def filter(predicate: (A => Boolean)): LambdaMyList[A] =
    if (predicate(h)) new LambdaCons(h, t.filter(predicate))
    else t.filter(predicate)

  def ++[B >: A](lst: LambdaMyList[B]): LambdaMyList[B] = new LambdaCons(h, t ++ lst)
}

object LambdaListTest extends App {

  /**
   * removed "new" as it is a case class
   * changed Function1 to lambda's
   *
   */
  val listOfIntegers = LambdaCons(1, LambdaCons(2, LambdaCons(3, LambdaEmpty)))
  val copyListOfIntegers = LambdaCons(1, LambdaCons(2, LambdaCons(3, LambdaEmpty)))
  val listOfStrings = LambdaCons("bibin", LambdaCons("nahas", LambdaCons("is awesome", LambdaEmpty)))

  println(listOfIntegers.head)
  println(listOfStrings.head)
  println(listOfStrings.add("kunjan").head)
  println(listOfIntegers.map((elem: Int) => elem * 2))
  println(listOfIntegers.filter((elem: Int) => elem % 2 == 0))
  println(listOfIntegers ++ listOfStrings)
  println(listOfIntegers.flatMap((elem: Int) => LambdaCons(elem, LambdaCons(elem + 1, LambdaEmpty))))
  println(copyListOfIntegers == listOfIntegers)

}

