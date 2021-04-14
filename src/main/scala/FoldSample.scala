object FoldSample {

  def appendFoo(s: String): String = s + "FOO"

  def makeUpper(s: String): String = s.toUpperCase

  def makeReverse(s: String): String = s.reverse

  def applyTransformationLeft(s: String, trans: Seq[String => String]): String = {
    trans.foldLeft(s) {
      (a, func) => func(a)
    }
  }

  def applyTransformationRight(trans: Seq[String => String], s: String): String = {
    trans.foldRight(s) {
      (func, a) => func(a)
    }
  }

  def main(args: Array[String]): Unit = {
    val resultLeft = applyTransformationLeft("bibin ", List(appendFoo, makeUpper, makeReverse))

    val resultRight = applyTransformationRight(List(appendFoo, makeUpper, makeReverse), "bibin ")

    println(resultLeft)
    println(resultRight)
  }

}
