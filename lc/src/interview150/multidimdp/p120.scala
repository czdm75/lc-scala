package p120

object Solution {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    triangle.tail
      .foldLeft(triangle.head) { case (sums, row) =>
        val left = (sums zip row).map { case (x, y) => x + y }
        val right = (sums zip row.tail).map { case (x, y) => x + y }
        (Int.MaxValue :: right)
          .zipAll(left, Int.MaxValue, Int.MaxValue)
          .map((math.min _).tupled)
      }
      .min
  }

  def main(args: Array[String]) = {
    println(minimumTotal(List(List(2), List(3, 4), List(6, 5, 7), List(4, 1, 8, 3))))
    println(minimumTotal(List(List(-10))))
    println(minimumTotal(List(List(-1), List(2, 3), List(1, -1, -3))))
  }
}
