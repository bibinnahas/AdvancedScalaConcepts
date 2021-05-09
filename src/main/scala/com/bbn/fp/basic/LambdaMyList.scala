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

  def foreach(f: (A => Unit)): Unit

  def sort(compare: (A, A) => Int): LambdaMyList[A]

  def zipWith[B, C](list: LambdaMyList[B], zip: ((A, B) => C)): LambdaMyList[C]

  def fold[B](start: B, f: ((A, B) => B)): B

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

  def foreach(f: Nothing => Unit): Unit = ()

  def sort(compare: (Nothing, Nothing) => Int): LambdaMyList[Nothing] = LambdaEmpty

  def zipWith[B, C](list: LambdaMyList[B], zip: (Nothing, B) => C): LambdaMyList[C] = {
    if (!list.isEmpty) throw new RuntimeException("List lengths differ!")
    else LambdaEmpty
  }

  override def fold[B](start: B, f: (Nothing, B) => B): B = start
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

  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def sort(compare: (A, A) => Int): LambdaMyList[A] = {
    val sortedTail = t.sort(compare)

    def insert(x: A, sortedList: LambdaMyList[A]): LambdaMyList[A] = {
      if (sortedList.isEmpty) new LambdaCons(x, LambdaEmpty)
      else if (compare(x, sortedList.head) <= 0) new LambdaCons(x, sortedList)
      else new LambdaCons(sortedList.head, insert(x, sortedList.tail))
    }

    insert(h, sortedTail)
  }

  def zipWith[B, C](list: LambdaMyList[B], zip: (A, B) => C): LambdaMyList[C] = {
    if (list.isEmpty) throw new RuntimeException("List lengths differ!")
    else new LambdaCons(zip(h, list.head), t.zipWith(list.tail, zip))
  }

  def fold[B](start: B, f: (A, B) => B): B = {
    val newStart = f(h, start)
    t.fold(newStart, f)
  }
}

object LambdaListTest extends App {

  /**
   * removed "new" as it is a case class
   * changed Function1 to lambda's
   *
   */
  val listOfIntegers = LambdaCons(1, LambdaCons(2, LambdaCons(3, LambdaEmpty)))
  val copyListOfIntegers = LambdaCons(3, LambdaCons(2, LambdaCons(1, LambdaEmpty)))
  val listOfStrings = LambdaCons("bibin", LambdaCons("nahas", LambdaCons("is awesome", LambdaEmpty)))

  println(listOfIntegers.head)
  println(listOfStrings.head)
  println(listOfStrings.add("kunjan").head)
  println(listOfIntegers.map((elem: Int) => elem * 2))
  println(listOfIntegers.filter((elem: Int) => elem % 2 == 0))
  println(listOfIntegers ++ listOfStrings)
  println(listOfIntegers.flatMap((elem: Int) => LambdaCons(elem, LambdaCons(elem + 1, LambdaEmpty))))
  println(copyListOfIntegers == listOfIntegers)

  //  special Function (Int => Func[Int, Int])
  val specialFunc: (Function1[Int, Function1[Int, Int]]) = new Function1[Int, Function1[Int, Int]] {
    override def apply(x: Int): Int => Int = new Function1[Int, Int] {
      override def apply(y: Int): Int = x * y
    }
  }
  val specialFuncModified: (Function1[Int, Function1[Int, Int]]) = new Function1[Int, Function1[Int, Int]] {
    override def apply(x: Int): (Int => Int) = (y: Int) => x * y
  }
  val specialFuncSimplified: (Function1[Int, Function1[Int, Int]]) = (x: Int) => (y: Int) => x * y
  val specialFuncFurtherSimplified: (Int => Int => Int) = (x: Int) => (y: Int) => x * y // [ ((Int) => (Int => Int)) ] = [ (x: Int) => ((y: Int) => x * y) ].
  // (Int => Int => Int) is right associative

  println(specialFuncFurtherSimplified(3)(4)) // curried function
  println(listOfIntegers.foreach(println))
  println(listOfIntegers.sort((x, y) => y - x))
  println(listOfIntegers.zipWith[Int, Int](copyListOfIntegers, _ * _)) //suppply [Int, Int] or do not use MOAR form
  println(listOfIntegers.fold(1, ((x: Int, y: Int) => x * y)))

}

