package p2566

object Solution {
  def minMaxDifference(num: Int): Int = {
    val minReplaced = (0 to 9).map { i =>
      num.toString.replace(i.toString, "0").toInt
    }.min

    val maxReplaced = (0 to 9).map { i =>
      num.toString.replace(i.toString, "9").toInt
    }.max

    maxReplaced - minReplaced
  }

  def main(args: Array[String]) = {
    println(minMaxDifference(11891))
    println(minMaxDifference(90))
  }
}
