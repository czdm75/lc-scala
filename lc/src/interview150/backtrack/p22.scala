package p22

object Solution {
  def generateParenthesis(n: Int): List[String] = {
    import scala.annotation.tailrec
    @tailrec
    def loop(candidates: List[(Int, Int, List[Char])], results: List[String]): List[String] = candidates match {
      // always l >= r
      case Nil                                     => results
      case (l, r, chars) :: tl if l == n && r == n => loop(tl, chars.reverse.mkString :: results)
      case (l, r, chars) :: tl if l == n           => loop((l, r + 1, ')' :: chars) :: tl, results)
      case (l, r, chars) :: tl if l == r           => loop((l + 1, r, '(' :: chars) :: tl, results)
      case (l, r, chars) :: tl => loop((l + 1, r, '(' :: chars) :: (l, r + 1, ')' :: chars) :: tl, results)
    }
    loop((1, 0, '(' :: Nil) :: Nil, Nil)
  }

  def main(args: Array[String]) = {
    println(generateParenthesis(3).mkString("\n"))
  }
}
