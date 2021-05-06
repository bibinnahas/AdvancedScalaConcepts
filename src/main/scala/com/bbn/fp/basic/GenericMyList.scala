package com.bbn.fp.basic

abstract class GenericMyList[+A] {
  def head: A

  def tail: GenericMyList[A]

  def isEmpty: Boolean

  def add[B >: A](elem: B): GenericMyList[B]

  def printElements: String

  override def toString: String = "[" + printElements + "]"

  def map[B](transformer: MyTransformer[A, B]): GenericMyList[B]

  def flatMap[B](transformer: MyTransformer[A, GenericMyList[B]]): GenericMyList[B]

  def filter(predicate: MyPredicate[A]): GenericMyList[A]

  def ++[B >: A](lst: GenericMyList[B]): GenericMyList[B]

}

object Empty extends GenericMyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: GenericMyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](elem: B): GenericMyList[B] = new Cons(elem, Empty)

  def printElements: String = ""

  def map[B](transformer: MyTransformer[Nothing, B]): GenericMyList[B] = Empty

  def flatMap[B](transformer: MyTransformer[Nothing, GenericMyList[B]]): GenericMyList[B] = Empty

  def filter(predicate: MyPredicate[Nothing]): GenericMyList[Nothing] = Empty

  def ++[B >: Nothing](lst: GenericMyList[B]): GenericMyList[B] = lst
}

class Cons[+A](h: A, t: GenericMyList[A]) extends GenericMyList[A] {
  def head: A = h

  def tail: GenericMyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](elem: B): GenericMyList[B] = new Cons(elem, this)

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  }

  def map[B](transformer: MyTransformer[A, B]): GenericMyList[B] =
    new Cons(transformer.transform(h), t.map(transformer))

  def flatMap[B](transformer: MyTransformer[A, GenericMyList[B]]): GenericMyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)

  def filter(predicate: MyPredicate[A]): GenericMyList[A] =
    if (predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def ++[B >: A](lst: GenericMyList[B]): GenericMyList[B] = new Cons(h, t ++ lst)
}

trait MyPredicate[-T] {
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(elem: A): B
}

object ListTest extends App {

  val listOfIntegers = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings = new Cons("bibin", new Cons("nahas", new Cons("is awesome", Empty)))

  println(listOfIntegers.head)
  println(listOfStrings.head)
  println(listOfStrings.add("kunjan").head)
  println(listOfIntegers.map(new MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }))
  println(listOfIntegers.filter(new MyPredicate[Int] {
    override def test(elem: Int): Boolean = elem % 2 == 0
  }))
  println(listOfIntegers ++ listOfStrings)
  println(listOfIntegers.flatMap(new MyTransformer[Int, GenericMyList[Int]] {
    override def transform(elem: Int): GenericMyList[Int] = new Cons(elem, new Cons(elem + 1, Empty))
  }))

}

