package p205

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def isIsomorphic(s: String, t: String): Boolean = {
    tailrecM(((s zip t).toList, Map.empty[Char, Char])) {
      case (Nil, _)                                              => Right(true)
      case ((x, y) :: tl, map) if map.contains(x) && map(x) != y => Right(false)
      case ((x, y) :: tl, map) if map.contains(x)                => Left((tl, map))
      case ((x, y) :: tl, map) if map.values.exists(_ == y)      => Right(false)
      case ((x, y) :: tl, map)                                   => Left(tl, map + (x -> y))
    }
  }

  def main(args: Array[String]) = {
    println(isIsomorphic("egg", "add"))
    println(isIsomorphic("foo", "bar"))
    println(isIsomorphic("paper", "title"))
    println(isIsomorphic("badc", "baba"))
  }
}
