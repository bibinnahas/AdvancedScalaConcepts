package com.bbn.fp.basic

object HOFExercises extends App {
  def toCurry(f: (Int, Int) => Int): (Int => Int => Int) = x => y => f(x, y)
  def fromCurry(f: (Int => Int => Int)): ((Int, Int) => Int) = (x, y) => f(x)(y)
  def compose[A, B, T](f: A => B, g: T => A): T => B = x => f(g(x))
  def deCompose[A, B, T](f: A => B, g: B => T): A => T = x => g(f(x))


}
