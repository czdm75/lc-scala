package p77

object Solution {
  def combine(n: Int, k: Int): List[List[Int]] = {
    val init = (1 to n).map(i => List(i)).toList
    (1 until k).foldLeft(init) { case (prefixes, _) =>
      for {
        prefix <- prefixes
        i <- (prefix.head + 1 to n)
      } yield i :: prefix
    }
  }

  def main(args: Array[String]) = {
    println(combine(4, 2))
  }
}
