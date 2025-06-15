package p5

object Solution {
  def longestPalindrome(s: String): String = {
    import scala.annotation.tailrec
    @tailrec
    def loop(s: String, lastIndices: Seq[(Int, Int)]): String = {
      val newIndices = lastIndices
        .filter { case (start, end) => start > 0 && end < s.length }
        .filter { case (start, end) => s(start - 1) == s(end) }
        .map { case (start, end) => (start - 1, end + 1) }
      if (newIndices.isEmpty) {
        Function.tupled(s.slice _)(lastIndices.head)
      } else {
        loop(s, newIndices)
      }
    }
    loop(s, s.indices.map(x => (x, x + 1)) ++ s.indices.map(x => (x, x)))
  }

  def main(args: Array[String]) = {
    println(longestPalindrome("babad"))
    println(longestPalindrome("cbbd"))

  }
}
