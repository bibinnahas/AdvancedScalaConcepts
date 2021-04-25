package com.bbn.fp.advanced

/**
 * Read Afsal Thaj's blog for what I tried to learn here
 * https://afsalthaj.github.io/myblog/posts/2021-04-15-fix-points.html
 *
 */
object FixPointTypes extends App {
  //  normal factorial
  val regFactorial: Int => Int =
    n => if (n == 0) 1 else n * regFactorial(n - 1)

  //  if name of function should not come in body
  def almostFactorial(f: Int => Int): Int => Int =
    n => if (n == 0) 1 else n * f(n - 1)

  //  factorial can be fixed point function if the below can be
  //  extended to infinity
  //  val factorial0: Int => Int =
  //  almostFactorial(identity)
  //  val factorial1: Int => Int =
  //    almostFactorial(factorial0)
  //  val factorial2: Int => Int =
  //    almostFactorial(factorial1)
  //  val factorial3: Int => Int =
  //    almostFactorial(factorial2)


  //  Solution: Y combinator
  //  STACK UNSAFE
  //  def Y[A](f: (A => A) => (A => A)): (A => A) = f(Y(f))
  //  stack SAFE
  def Y[A](f: (A => A) => (A => A)): (A => A) = f(a => Y(f)(a))

  //  val factorial = Y(almostFactorial)
  //  println(factorial(10))

  //  create partFactorial and making it simpler from v0 => v4
  def partFactorialv0(self: Any): Int => Int = {
    val f = self.asInstanceOf[Any => (Int => Int)](self)
    n => if (n == 0) 1 else n * f(n - 1)
  }

  def partFactorialv1(self: Any): Int => Int = {
    val selfSelf = self.asInstanceOf[Any => (Int => Int)](self)
    almostFactorial(selfSelf)
  }

  //  println(partFactorialv1(partFactorialv1 _)) // STACK U NSAFE
  //  Write partfactorial as factorial
  def factorialv1: Int => Int = {
    val partFactorial =
      (self: Any) => almostFactorial(self.asInstanceOf[Any => (Int => Int)](self))
    partFactorial(partFactorial)
  }

  def factorialv2: Int => Int = {
    val x = // partFactorial ---> x
      (self: Any) => almostFactorial(self.asInstanceOf[Any => (Int => Int)](self))
    x(x)
  }

  def factorialv3: Int => Int = {
    val x: Any => (Int => Int) = // added return type
      (self: Any) => almostFactorial(self.asInstanceOf[Any => (Int => Int)](self))
    x(x)
  }

  def factorialv4: Int => Int = {
    ((x: Any => (Int => Int)) => x(x))(
      (x: Any) => almostFactorial(x.asInstanceOf[Any => (Int => Int)](x)) // renames self to x and added lambda notations
    )
  }





}
