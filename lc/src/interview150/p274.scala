package p274

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def hIndex(citations: Array[Int]): Int = {
    val citationCnt = Array.fill(citations.length + 1)(0)
    citations.foreach { x =>
      val idx = math.min(citations.length, x)
      citationCnt(idx) += 1
    }
    tailrecM((citations.size, 0)) { case (n, higher) =>
      val higherOrEq = citationCnt(n) + higher
      if (higherOrEq >= n) {
        Right(n)
      } else {
        Left(n - 1, higherOrEq)
      }
    }
  }

  def main(args: Array[String]) = {
    println(hIndex(Array(3, 0, 6, 1, 5)))
    println(hIndex(Array(1, 3, 1)))
  }
}
