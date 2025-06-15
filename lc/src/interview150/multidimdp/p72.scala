package p72

object Solution {
  def minDistanceMut(word1: String, word2: String): Int = {
    val dp = Array.fill(word1.length + 1)(Array.fill(word2.length + 1)(0))
    // first row
    (0 to word2.length) foreach { j =>
      dp(0)(j) = j
    }
    (1 to word1.length) foreach { i =>
      (0 to word2.length) foreach { j =>
        if (j == 0) {
          dp(i)(0) = i
        } else {
          val fromLeft = dp(i)(j - 1) + 1
          val fromAbove = dp(i - 1)(j) + 1
          val fromDiag = dp(i - 1)(j - 1) + (if word1(i - 1) == word2(j - 1) then 0 else 1)
          dp(i)(j) = math.min(fromLeft, math.min(fromAbove, fromDiag))
        }
      }
    }
    dp.last.last
  }

  def minDistance(word1: String, word2: String): Int = {
    (0 to word1.length)
      .foldLeft(Seq.fill(word2.length + 1)(0)) { case (lastRow, i) =>
        lastRow.indices
          .scanLeft(0) { case (left, j) =>
            (i, j) match {
              case (0, 0) => 0
              case (0, _) => j
              case (_, 0) => i
              case _ => {
                val fromLeft = left + 1
                val fromAbove = lastRow(j) + 1
                val fromDiag = lastRow(j - 1) + (if word1(i - 1) == word2(j - 1) then 0 else 1)
                Seq(fromLeft, fromAbove, fromDiag).min
              }
            }
          }.tail
      }.last
  }

  def main(args: Array[String]) = {
    println(minDistance(word1 = "horse", word2 = "ros"))
    println(minDistance(word1 = "intention", word2 = "execution"))
  }
}
