object FoldLeftSample {

  def appendFoo(s: String): String = s + "FOO"

  def makeUpper(s: String): String = s.toUpperCase

  def makeReverse(s: String): String = s.reverse

  def applyTransformation(s: String, trans: Seq[String => String]): String = {
    trans.foldLeft(s) {
      (a, func) => func(a)
    }
  }

  def main(args: Array[String]): Unit = {
    val result = applyTransformation("bibin ", List(appendFoo, makeUpper, makeReverse))

    println(result)
  }

}
