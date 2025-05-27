package p149

object Solution {
  def maxPoints(points: Array[Array[Int]]): Int = {
    def sameLine(i1: Int, i2: Int, i3: Int): Boolean = {
      val Array(x1, y1) = points(i1)
      val Array(x2, y2) = points(i2)
      val Array(x3, y3) = points(i3)
      if (y1 == y2 && y1 == y3) {
        true
      } else if (x1 == x2 && x1 == x3) {
        true
      } else if (y1 == y2 || x1 == x2 || y1 == y3 || x1 == x3) {
        false
      } else if ((y1 - y2) * (x1 - x3) == (y1 - y3) * (x1 - x2)) {
        true
      } else {
        false
      }
    }

    if (points.size == 1) {
      1
    } else
      (for {
        i <- points.indices
        j <- points.indices if i != j
      } yield {
        points.indices.foldLeft(2) {
          case (n, idx) if i == idx || j == idx  => n
          case (n, idx) if (sameLine(i, j, idx)) => n + 1
          case (n, _)                            => n
        }
      }).max
  }

  def main(args: Array[String]) = {
    println(maxPoints(Array(1, 1, 3, 2, 5, 3, 4, 1, 2, 3, 1, 4).grouped(2).toArray))
  }
}
