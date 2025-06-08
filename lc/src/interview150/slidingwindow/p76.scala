package p76

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def minWindow(s: String, t: String): String = {
    val map = t.groupMapReduce(identity)(_ => 1)(_ + _)
    tailrecM((0, 0, Map.empty[Char, Int], Option.empty[(Int, Int)])) { case (start, end, m, resultOpt) =>
      if (map.exists { case (char, cnt) => m.getOrElse(char, 0) < cnt }) {
        // lacking any char
        if (end == s.length) {
          Right(resultOpt.map(s.substring.tupled).getOrElse(""))
        } else {
          val nextChar = s(end)
          val nextCharCnt = m.getOrElse(nextChar, 0) + 1
          val nextM = m + (nextChar -> nextCharCnt)
          Left((start, end + 1, nextM, resultOpt))
        }
      } else {
        // all chars fullfilled
        val firstChar = s(start)
        val firstCharCnt = m(firstChar) - 1
        val nextM = if firstCharCnt == 0 then m - firstChar else m + (firstChar -> firstCharCnt)
        val nextResultOpt = resultOpt match {
          case None                                                                      => Some((start, end))
          case Some(resultStart, resultEnd) if (resultEnd - resultStart) > (end - start) => Some((start, end))
          case otherwise                                                                 => otherwise
        }
        Left((start + 1, end, nextM, nextResultOpt))
      }
    }
  }

  def main(args: Array[String]) = {
    println(minWindow(s = "ADOBECODEBANC", t = "ABC"))
    println(minWindow(s = "a", t = "a"))
    println(minWindow(s = "a", t = "aa"))
  }
}
