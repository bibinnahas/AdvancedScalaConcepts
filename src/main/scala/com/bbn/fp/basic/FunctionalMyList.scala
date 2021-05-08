package com.bbn.fp.basic

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

case object FuncEmpty extends FunctionalMyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: FunctionalMyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](elem: B): FunctionalMyList[B] = new FuncCons(elem, FuncEmpty)

  def printElements: String = ""

  def map[B](transformer: (Nothing => B)): FunctionalMyList[B] = FuncEmpty

  def flatMap[B](transformer: (Nothing => FunctionalMyList[B])): FunctionalMyList[B] = FuncEmpty

  def filter(predicate: (Nothing => Boolean)): FunctionalMyList[Nothing] = FuncEmpty

  def ++[B >: Nothing](lst: FunctionalMyList[B]): FunctionalMyList[B] = lst
}

case class FuncCons[+A](h: A, t: FunctionalMyList[A]) extends FunctionalMyList[A] {
  def head: A = h

  def tail: FunctionalMyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](elem: B): FunctionalMyList[B] = new FuncCons(elem, this)

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  }

  def map[B](transformer: (A => B)): FunctionalMyList[B] =
    new FuncCons(transformer(h), t.map(transformer))

  def flatMap[B](transformer: (A => FunctionalMyList[B])): FunctionalMyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  def filter(predicate: (A => Boolean)): FunctionalMyList[A] =
    if (predicate(h)) new FuncCons(h, t.filter(predicate))
    else t.filter(predicate)

  def ++[B >: A](lst: FunctionalMyList[B]): FunctionalMyList[B] = new FuncCons(h, t ++ lst)
}

//trait MyPredicate[-T] {
//  def test(elem: T): Boolean
//}
//
//trait MyTransformer[-A, B] {
//  def transform(elem: A): B
//}

object FunctionalListTest extends App {

  val listOfIntegers = new FuncCons(1, new FuncCons(2, new FuncCons(3, FuncEmpty)))
  val copyListOfIntegers = new FuncCons(1, new FuncCons(2, new FuncCons(3, FuncEmpty)))
  val listOfStrings = new FuncCons("bibin", new FuncCons("nahas", new FuncCons("is awesome", FuncEmpty)))

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
    override def apply(elem: Int): FunctionalMyList[Int] = new FuncCons(elem, new FuncCons(elem + 1, FuncEmpty))
  }))
  println(copyListOfIntegers == listOfIntegers)

}

