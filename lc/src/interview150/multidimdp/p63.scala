package p63

object Solution {
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    if (obstacleGrid.head.head == 1) {
      0
    } else {
      val (m, n) = (obstacleGrid.size, obstacleGrid.head.size)
      obstacleGrid(0)(0) = 1
      (1 to (m + n - 1)).foreach { i =>
        (0 until m)
          .map(x => (x, i - x))
          .filter { case (x, y) => x >= 0 && y >= 0 && x < m && y < n }
          .foreach { case (x, y) =>
            obstacleGrid(x)(y) = (x, y) match {
              case (x, y) if (obstacleGrid(x)(y) == 1) => 0
              case (0, y)                              => obstacleGrid(x)(y - 1)
              case (x, 0)                              => obstacleGrid(x - 1)(y)
              case (x, y)                              => obstacleGrid(x - 1)(y) + obstacleGrid(x)(y - 1)
            }
          }
      }
      obstacleGrid.last.last
    }
  }

  def main(args: Array[String]) = {
    println(uniquePathsWithObstacles(Array(Array(0, 0, 0), Array(0, 1, 0), Array(0, 0, 0))))
  }
}
