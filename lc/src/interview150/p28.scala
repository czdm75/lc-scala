package p28

object Solution {
  def strStr(haystack: String, needle: String): Int = {
    needle.zipWithIndex
      .foldLeft[Seq[Int]](0 to (haystack.length - needle.length)) {
        case (starts, (c, idx)) =>
          starts.filter { start => haystack(start + idx) == c }
      }
      .headOption
      .getOrElse(-1)
  }

  def main(args: Array[String]) = {
    println(strStr(haystack = "sadbutsad", needle = "sad"))
    println(strStr(haystack = "leetcode", needle = "leeto"))
  }
}
