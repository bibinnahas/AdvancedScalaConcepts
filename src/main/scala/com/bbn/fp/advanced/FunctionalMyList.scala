package com.bbn.fp.advanced

abstract class FunctionalMyList[+A] {
  def head: A

  def tail: FunctionalMyList[A]

  def isEmpty: Boolean

  def add[B >: A](elem: B): FunctionalMyList[B]

  def printElements: String

  override def toString: String = "[" + printElements + "]"

  def map[B](transformer: (A => B)): FunctionalMyList[B]

  def flatMap[B](transformer: (A => FunctionalMyList[B])): FunctionalMyList[B]

  def filter(predicate: (A => Boolean)): FunctionalMyList[A]

  def ++[B >: A](lst: FunctionalMyList[B]): FunctionalMyList[B]

}

case object Empty extends FunctionalMyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: FunctionalMyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](elem: B): FunctionalMyList[B] = new Cons(elem, Empty)

  def printElements: String = ""

  def map[B](transformer: (Nothing => B)): FunctionalMyList[B] = Empty

  def flatMap[B](transformer: (Nothing => FunctionalMyList[B])): FunctionalMyList[B] = Empty

  def filter(predicate: (Nothing => Boolean)): FunctionalMyList[Nothing] = Empty

  def ++[B >: Nothing](lst: FunctionalMyList[B]): FunctionalMyList[B] = lst
}

case class Cons[+A](h: A, t: FunctionalMyList[A]) extends FunctionalMyList[A] {
  def head: A = h

  def tail: FunctionalMyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](elem: B): FunctionalMyList[B] = new Cons(elem, this)

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  }

  def map[B](transformer: (A => B)): FunctionalMyList[B] =
    new Cons(transformer(h), t.map(transformer))

  def flatMap[B](transformer: (A => FunctionalMyList[B])): FunctionalMyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  def filter(predicate: (A => Boolean)): FunctionalMyList[A] =
    if (predicate(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def ++[B >: A](lst: FunctionalMyList[B]): FunctionalMyList[B] = new Cons(h, t ++ lst)
}

//trait MyPredicate[-T] {
//  def test(elem: T): Boolean
//}
//
//trait MyTransformer[-A, B] {
//  def transform(elem: A): B
//}

object FunctionalListTest extends App {

  val listOfIntegers = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val copyListOfIntegers = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings = new Cons("bibin", new Cons("nahas", new Cons("is awesome", Empty)))

  println(listOfIntegers.head)
  println(listOfStrings.head)
  println(listOfStrings.add("kunjan").head)
  println(listOfIntegers.map(new Function1[Int, Int] {
    override def apply(elem: Int): Int = elem * 2
  }))
  println(listOfIntegers.filter(new Function[Int, Boolean] {
    override def apply(elem: Int): Boolean = elem % 2 == 0
  }))
  println(listOfIntegers ++ listOfStrings)
  println(listOfIntegers.flatMap(new Function1[Int, FunctionalMyList[Int]] {
    override def apply(elem: Int): FunctionalMyList[Int] = new Cons(elem, new Cons(elem + 1, Empty))
  }))
  println(copyListOfIntegers == listOfIntegers)

}

