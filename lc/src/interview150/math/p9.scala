package p9

object Solution {
  def isPalindrome(x: Int): Boolean = {
    val digits = Iterator.iterate(x)(_ / 10).takeWhile(_ != 0).map(_ % 10).toSeq
    return x >= 0 && digits.reverse == digits
  }
}
