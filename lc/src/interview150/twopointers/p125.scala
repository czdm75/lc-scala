package p125

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def isPalindrome(s: String): Boolean = {
    tailrecM((false, 0, false, s.length - 1)) {
      case (_, p1, _, p2) if p1 > p2 || p1 > s.length || p2 < 0 => Right(true)
      case (true, p1, true, p2) if s(p1).toLower == s(p2).toLower =>
        Left(false, p1 + 1, false, p2 - 1)
      case (true, _, true, _) => Right(false)
      case (false, p1, f2, p2) if s(p1).isLetterOrDigit =>
        Left(true, p1, f2, p2)
      case (false, p1, f2, p2) => Left(false, p1 + 1, f2, p2)
      case (true, p1, false, p2) if s(p2).isLetterOrDigit =>
        Left(true, p1, true, p2)
      case (true, p1, false, p2) => Left(true, p1, false, p2 - 1)
    }
  }

  def main(args: Array[String]) = {
    println(isPalindrome("A man, a plan, a canal: Panama"))
    println(isPalindrome("race a car"))
    println(isPalindrome(" "))
  }
}
