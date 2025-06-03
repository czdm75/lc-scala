package p97

object Solution {
  def isInterleave(s1: String, s2: String, s3: String): Boolean = {
    // 对 s3 的每一个位置，其可能只能取 s1，可能只能取 s2，可能都能取，可能不能取（舍弃）。因此要保留多种可能情况
    // 如果将 s1 s2 作为 x y 轴，相当于以只能向下、向右的方向走出一条 path。
    s3.foldLeft(Seq((0, 0))) { case (candidates, c) =>
      candidates.flatMap { case (i1, i2) =>
        (i1 < s1.size && s1(i1) == c, i2 < s2.size && s2(i2) == c) match {
          case (true, true)   => Seq((i1 + 1, i2), (i1, i2 + 1))
          case (true, false)  => Seq((i1 + 1, i2))
          case (false, true)  => Seq((i1, i2 + 1))
          case (false, false) => Seq.empty
        }
      }
    }.nonEmpty
  }

  def main(args: Array[String]) = {
    println(isInterleave(s1 = "aabcc", s2 = "dbbca", s3 = "aadbbcbcac"))
    println(isInterleave(s1 = "aabcc", s2 = "dbbca", s3 = "aadbbbaccc"))
    println(isInterleave(s1 = "", s2 = "", s3 = ""))
  }
}
