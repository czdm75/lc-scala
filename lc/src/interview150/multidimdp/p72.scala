package p72

object Solution {
  def minDistance(word1: String, word2: String): Int = {
    // 将两个 word 作为 x y 轴，对每个组合计算编辑距离
    // 对于原点，编辑距离是 0
    // 对于x = 0 或 y = 0，编辑距离是 n
    // 对于其他位置，取左、上、左上三种方式中较小的
    val dp = Array.fill(word1.length)(Array.fill(word2.length))
    word1.indices.foreach { i => dp(i)(0) = i }
    word2.indices.foreach { i => dp(0)(i) = i }
    word2.indices.tail.foreach { y =>
      word1.indices.tail.foreach { x =>
        val leftup = if word1(x) == word2(y) then 0 else 1
        dp(x)(y) = math.min(math.min(dp(x - 1)(y), dp(x)(y - 1)), dp(x - 1)(y - 1) + leftup)
      }
    }
    dp.last.last
  }

  def main(args: Array[String]) = {
    println(minDistance(word1 = "horse", word2 = "ros"))
    println(minDistance(word1 = "intention", word2 = "execution"))
  }
}
