package p73

object Solution {
  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    val zeroPositions = for {
      i <- matrix.indices
      j <- matrix(0).indices if matrix(i)(j) == 0
    } yield (i, j)
    for ((i, j) <- zeroPositions) {
      for (x <- matrix(0).indices) {
        matrix(i)(x) = 0
      }
      for (x <- matrix.indices) {
        matrix(x)(j) = 0
      }
    }
  }
}
