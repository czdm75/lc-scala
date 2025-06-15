package p97

object Solution {
  def isInterleave(s1: String, s2: String, s3: String): Boolean = {
    // x and y are index of **Next** char to append, so (0, 0) means a empty string.
    s3.foldLeft(Seq((0, 0))) { case (candidates, c) =>
      candidates.flatMap { case (x, y) =>
        (x < s1.length && s1(x) == c, y < s2.length && s2(y) == c) match {
          case (true, true)  => Seq((x + 1, y), (x, y + 1))
          case (true, false) => Seq((x + 1, y))
          case (false, true) => Seq((x, y + 1))
          case _             => Seq.empty
        }
      }.distinct
    } == Seq((s1.size, s2.size))
  }

  def isInterleaveMut(s1: String, s2: String, s3: String): Boolean = {
    if (s1.length + s2.length != s3.length) {
      false
    } else {
      val dp = Array.fill(s1.length + 1)(Array.fill(s2.length + 1)(false))
      (0 to s1.length) foreach { i =>
        (0 to s2.length) foreach { j =>
          dp(i)(j) = (i, j) match {
            case (0, 0) => true
            case _ => {
              val fromLeft = j > 0 && dp(i)(j - 1) && s2(j - 1) == s3(i + j - 1)
              val fromAbove = i > 0 && dp(i - 1)(j) && s1(i - 1) == s3(i + j - 1)
              fromLeft || fromAbove
            }
          }
        }
      }
      dp.last.last
    }
  }

  def isInterleaveDP(s1: String, s2: String, s3: String): Boolean = {
    if (s1.length + s2.length != s3.length) {
      false
    } else {
      (0 to s1.length).foldLeft(Seq.fill(s2.length + 1)(false)) {
        case (lastRow, i) =>
          lastRow.indices.scanLeft(false) { case (left, j) =>
            val isInit = i == 0 && j == 0
            val fromLeft = j > 0 && left && s2(j - 1) == s3(i + j - 1)
            val fromAbove = i > 0 && lastRow(j) && s1(i - 1) == s3(i + j - 1)
            isInit || fromLeft || fromAbove
        }.tail
      }.last
    }
  }

  def main(args: Array[String]) = {
    println(isInterleaveDP(s1 = "aabcc", s2 = "dbbca", s3 = "aadbbcbcac"))
    println(isInterleaveDP(s1 = "aabcc", s2 = "dbbca", s3 = "aadbbbaccc"))
    println(isInterleaveDP(s1 = "", s2 = "", s3 = ""))
    println(isInterleaveDP(s1 = "a", s2 = "b", s3 = "a"))
  }
}
