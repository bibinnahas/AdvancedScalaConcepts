import scala.annotation.tailrec

object SimpleRecursion extends App {

  def factorial(n: Int): Long = {
    @tailrec
    def factorialHelper(n: Int, acc: Long): Long = {
      if (n == 1) acc
      else factorialHelper(n - 1, n * acc)
    }

    factorialHelper(n, 1)
  }

  println(factorial(55))
}
