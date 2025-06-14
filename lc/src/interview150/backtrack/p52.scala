package p52

object Solution {
  def totalNQueens(n: Int): Int = {
    import scala.annotation.tailrec

    def attacks(i: Int, j: Int): Boolean = {
      val (x1, y1) = (i / n, i % n)
      val (x2, y2) = (j / n, j % n)
      x1 == x2 || y1 == y2 || (x1 + y1) == (x2 + y2) || (x1 - y1) == (x2 - y2)
    }

    // in order to make solution distinct, we use 1D cordinates and make cordinate increase-only
    @tailrec
    def loop(places: List[List[Int]], result: Int): Int = places match {
      case Nil                      => result
      case hd :: tl if hd.size == n => loop(tl, result + 1)
      case hd :: tl => {
        val newPlaces = (hd.head + 1 until n * n)
          .filter { x => !hd.exists(attacks(x, _)) }
          .map(_ :: hd)
        loop(tl prependedAll newPlaces, result)
      }
    }
    loop((0 until n * n).map(_ :: Nil).toList, 0)
  }

  def main(args: Array[String]) = {
    println((1 to 9).map(totalNQueens).mkString("\n"))
  }
}
