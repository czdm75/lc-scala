package p290

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def wordPattern(pattern: String, s: String): Boolean = {
    tailrecM((pattern.zipAll(s.split(" "), '\u0000', "").toList, Map.empty[Char, String])) {
      case (Nil, _)                                              => Right(true)
      case (('\u0000', _) :: tl, _)                              => Right(false)
      case ((_, "") :: tl, _)                                    => Right(false)
      case ((x, y) :: tl, map) if map.contains(x) && map(x) != y => Right(false)
      case ((x, y) :: tl, map) if map.contains(x)                => Left((tl, map))
      case ((x, y) :: tl, map) if map.values.exists(_ == y)      => Right(false)
      case ((x, y) :: tl, map)                                   => Left((tl, map + (x -> y)))
    }
  }

  def main(args: Array[String]) = {
    println(wordPattern(pattern = "abba", s = "dog cat cat dog"))
    println(wordPattern(pattern = "abba", s = "dog cat cat fish"))
    println(wordPattern(pattern = "aaaa", s = "dog cat cat dog"))
    println(wordPattern(pattern = "aaa", s = "aa aa aa aa"))
  }
}
