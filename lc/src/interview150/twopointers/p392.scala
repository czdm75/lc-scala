package p392

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def isSubsequence(s: String, t: String): Boolean = {
    tailrecM((0, 0)) {
      case (p1, _) if p1 == s.length  => Right(true)
      case (_, p2) if p2 == t.length  => Right(false)
      case (p1, p2) if s(p1) == t(p2) => Left((p1 + 1, p2 + 1))
      case (p1, p2)                   => Left((p1, p2 + 1))
    }
  }

  def main(args: Array[String]) = {
    println(isSubsequence(s = "abc", t = "ahbgdc"))
    println(isSubsequence(s = "axc", t = "ahbgdc"))
  }
}
