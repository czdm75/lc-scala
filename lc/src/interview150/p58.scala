package p58

object Solution {
  def lengthOfLastWord(s: String): Int = {
    s.reverseIterator.dropWhile(_ == ' ').takeWhile(_ != ' ').size
  }
}
