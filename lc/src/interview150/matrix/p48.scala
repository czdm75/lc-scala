package p48

object Solution {
  def rotate(matrix: Array[Array[Int]]): Unit = {
    val n = matrix.length
    (0 to (n - 1) / 2 + 1).foreach { layer =>
      (0 to (n - layer - layer - 2)).foreach { offset =>
        val (v1, v2, v3, v4) = (
          matrix(layer)(layer + offset),
          matrix(layer + offset)(n - 1 - layer),
          matrix(n - layer - 1)(n - layer - 1 - offset),
          matrix(n - layer - 1 - offset)(layer)
        )
        matrix(layer)(layer + offset) = v4
        matrix(layer + offset)(n - 1 - layer) = v1
        matrix(n - layer - 1)(n - layer - 1 - offset) = v2
        matrix(n - layer - 1 - offset)(layer) = v3
      }
    }
  }

  def main(args: Array[String]) = {
    val mat1 = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
    rotate(mat1)
    println(mat1.map(_.mkString(" ")).mkString("\n"))
    println()
    val mat2 = Array(Array(5, 1, 9, 11), Array(2, 4, 8, 10), Array(13, 3, 6, 7), Array(15, 14, 12, 16))
    rotate(mat2)
    println(mat2.map(_.mkString(" ")).mkString("\n"))
  }
}
