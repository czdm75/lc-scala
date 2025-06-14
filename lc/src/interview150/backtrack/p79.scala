package p79

object Solution {
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    import scala.annotation.tailrec

    @tailrec
    def loop(candidates: List[List[(Int, Int)]]): Boolean = candidates match {
      case Nil                                  => false
      case hd :: tl if hd.length == word.length => true
      case (hd @ ((x, y) :: prev)) :: tl => {
        val nexts = Seq((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
          .filterNot(prev.contains)
          .filter { case (x, _) => x >= 0 && x < board.size }
          .filter { case (_, y) => y >= 0 && y < board.head.size }
          .filter { case (x, y) => board(x)(y) == word(hd.length) }
          .map(_ :: hd)
        loop(tl prependedAll nexts)
      }
      case _ => ???
    }

    val inits = for {
      x <- board.indices
      y <- board.head.indices
      if board(x)(y) == word.head
    } yield List((x, y))

    loop(inits.toList)
  }

  def main(args: Array[String]) = {
    println(exist("ABCESFCSADEE".toArray.grouped(4).toArray, "ABCCED"))
  }
}
