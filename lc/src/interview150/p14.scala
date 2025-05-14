package p14

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def longestCommonPrefix(strs: Array[String]): String = {
    tailrecM((0, List.empty[Char])) { case (idx, chars) =>
      if (strs.forall(_.length > idx)) {
        val cs: Array[Char] = strs.map(_(idx)).distinct
        cs match {
          case Array(c) => Left((idx + 1, c :: chars))
          case _        => Right(chars.reverse.mkString)
        }
      } else {
        Right(chars.reverse.mkString)
      }
    }
  }

  def main(args: Array[String]) = {
    println(longestCommonPrefix(Array("flower", "flow", "flight")))
    println(longestCommonPrefix(Array("dog", "racecar", "car")))
    println(longestCommonPrefix(Array("", "b")))
  }
}
