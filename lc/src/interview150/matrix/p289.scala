package p289

object Solution {
  def gameOfLife(board: Array[Array[Int]]): Unit = {
    val (m, n) = (board.length, board.head.length)
    // use extra states to do it in-place: 2 means 0 -> 1, 3 means 1 -> 0
    board.indices foreach { x =>
      board.head.indices foreach { y =>
        val neighbors = Seq(
          if x == 0     || y == 0     then 0 else board(x - 1)(y - 1) % 2,
          if x == 0     || y == n - 1 then 0 else board(x - 1)(y + 1) % 2,
          if x == m - 1 || y == 0     then 0 else board(x + 1)(y - 1) % 2,
          if x == m - 1 || y == n - 1 then 0 else board(x + 1)(y + 1) % 2,
          if x == 0                   then 0 else board(x - 1)(y    ) % 2,
          if x == m - 1               then 0 else board(x + 1)(y    ) % 2,
          if y == 0                   then 0 else board(x    )(y - 1) % 2,
          if y == n - 1               then 0 else board(x    )(y + 1) % 2,
        ).sum
        board(x)(y) = (board(x)(y), neighbors) match {
            case (0, 3) => 2
            case (0, _) => 0
            case (1, 2) => 1
            case (1, 3) => 1
            case (1, _) => 3
            case _ => throw new IllegalStateException("")
        }
      }
    }
    board.indices foreach { x =>
      board.head.indices foreach { y =>
        board(x)(y) = board(x)(y) match {
            case 2 => 1
            case 3 => 0
            case n => n
        }
      }
    }
  }
}
