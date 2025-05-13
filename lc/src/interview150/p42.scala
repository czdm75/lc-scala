package p42

object Solution {
  def trap(height: Array[Int]): Int = {
    val leftHigh = height.scanLeft(0)(math.max).init
    val rightHigh =
      height.reverseIterator.scanLeft(0)(math.max).toSeq.reverse.tail
    (leftHigh zip rightHigh zip height).map { case ((l, r), c) =>
      math.max(math.min(l, r) - c, 0)
    }.sum
  }

  def main(args: Array[String]) = {
    println(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)))
    println(trap(Array(4, 2, 0, 3, 2, 5)))
  }
}
