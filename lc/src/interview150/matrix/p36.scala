package p36

object Solution {
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    def valid(chars: Array[Char]) = {
      val cs = chars.filterNot(_ == '.')
      cs.distinct.length == cs.length
    }
    val horizontal = for (i <- 0 until 9) yield board(i)
    val vertical = for (i <- 0 until 9) yield board.map(_.apply(i))
    val blocks = for {
      x <- 0 until 9 by 3
      y <- 0 until 9 by 3
    } yield { board.slice(x, x + 3).flatMap(_.slice(y, y + 3)) }
    (horizontal ++ vertical ++ blocks).map(valid).reduce(_ && _)
  }

  def main(args: Array[String]) = {
    val board1 = Array(
      Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
    )
    val board2 = Array(
      Array('8', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
    )
    println(isValidSudoku(board1))
    println(isValidSudoku(board2))
  }
}
