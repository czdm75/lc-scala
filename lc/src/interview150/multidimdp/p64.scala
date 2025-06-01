package p64

object Solution {
  def minPathSum(grid: Array[Array[Int]]): Int = {
    // mutabe implementation, use grid as cache
    val size = grid.size + grid.head.size - 1
    (1 until size).foreach { step =>
      val cordinates = ((0 to step) zip (0 to step).reverse).filter { case (x, y) =>
        x >= 0 && y >= 0 && x < grid.size && y < grid.head.size
      }

      println(cordinates.toList)
      cordinates.foreach {
        case (x, y) => {
          val left = if (x == 0) Int.MaxValue else grid(x - 1)(y)
          val up = if (y == 0) Int.MaxValue else grid(x)(y - 1)
          grid(x)(y) = math.min(left, up) + grid(x)(y)
        }
      }
    }
    println(grid.map(_.mkString(" ")).mkString("\n"))
    grid.last.last
  }
  def main(args: Array[String]) = {
    println(minPathSum(Array(1, 3, 1, 1, 5, 1, 4, 2, 1).grouped(3).toArray))
  }
}
