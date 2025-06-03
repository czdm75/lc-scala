package p5

object Solution {
  def longestPalindrome(s: String): String = {
    // 生成一个序列，其中位置 i 保存坐标 n，使得 [n, i] 是一个回文串。
    // 对每个位置，基于上一个位置，检查当前的回文串
    val (from, to) = s.indices
      .foldLeft(List.empty[Int]) {
        case (Nil, i)                                   => i :: Nil
        case (l :: tl, i) if l == i - 1 && s(i) == s(l) => l :: l :: tl
        case (0 :: tl, i)                               => i :: 0 :: tl
        case (l :: tl, i) if s(i) == s(l - 1)           => (l - 1) :: l :: tl
        case (l :: tl, i)                               => i :: l :: tl
      }
      .reverse
      .zipWithIndex
      .maxBy { case (x, y) => y - x }
    s.slice(from, to + 1)
  }

  def main(args: Array[String]) = {
    println(longestPalindrome("babad"))
    println(longestPalindrome("cbbd"))

  }
}
