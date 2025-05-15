package p135

object Solution {
  def candy(ratings: Array[Int]): Int = {
    val leftLower = ratings.view
      .scanLeft((0, ratings.head)) { case ((cnt, left), value) =>
        if (value > left) {
          (cnt + 1, value)
        } else {
          (0, value)
        }
      }
      .map(_._1)
      .tail
    val rightLower = ratings.reverseIterator
      .scanLeft((0, ratings.last)) { case ((cnt, right), value) =>
        if (value > right) {
          (cnt + 1, value)
        } else {
          (0, value)
        }
      }
      .map(_._1)
      .toSeq
      .reverse
      .init
    return (leftLower zip rightLower)
      .map((x, y) => math.max(x, y))
      .sum + ratings.length
  }

  def main(args: Array[String]) = {
    println(candy(Array(1, 0, 2)))
    println(candy(Array(1, 2, 2)))
  }
}
