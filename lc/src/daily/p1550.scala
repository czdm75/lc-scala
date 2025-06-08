package p1550

object Solution {
  def threeConsecutiveOdds(arr: Array[Int]): Boolean = {
    (arr.iterator zip arr.iterator.drop(1) zip arr.iterator.drop(2)).exists { case ((x, y), z) =>
      println(s"$x, $y, $z")
      x % 2 == 1 && y % 2 == 1 && z % 2 == 1
    }
  }

  def main(args: Array[String]) = {
    println(threeConsecutiveOdds(Array(2, 6, 4, 1)))
  }
}
