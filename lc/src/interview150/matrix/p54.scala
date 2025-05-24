package p54

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    val (m, n) = (matrix.length, matrix.head.length)
    tailrecM((0, 0, 0, 0, 0, List.empty[Int])) { case (idx, x, y, dir, layer, list) =>
      if (idx == m * n) {
        Right(list.reverse)
      } else {
        val nextList = matrix(x)(y) :: list
        val (xx, yy, dd, ll) = dir match {
          // check turns
          case 0 if y == n - layer - 1 => (x + 1, y, 1, layer)
          case 1 if x == m - layer - 1 => (x, y - 1, 2, layer)
          case 2 if y == layer         => (x - 1, y, 3, layer)
          case 3 if x == layer + 1     => (x, y + 1, 0, layer + 1)
          // continue
          case 0 => (x, y + 1, 0, layer)
          case 1 => (x + 1, y, 1, layer)
          case 2 => (x, y - 1, 2, layer)
          case 3 => (x - 1, y, 3, layer)
        }
        Left((idx + 1, xx, yy, dd, ll, nextList))
      }
    }
  }

  def main(args: Array[String]) = {
    println(spiralOrder(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))).mkString(" "))
    println(spiralOrder(Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12))).mkString(" "))
  }
}
