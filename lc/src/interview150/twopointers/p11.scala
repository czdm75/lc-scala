package p11

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def maxArea(height: Array[Int]): Int = {
    tailrecM((0, 0, height.length - 1)) {
      case (max, l, r) if l == r => Right(max)
      case (max, l, r) =>
        val m = math.max(max, math.min(height(l), height(r)) * (r - l))
        if height(l) > height(r) then Left((m, l, r - 1))
        else Left((m, l + 1, r))
    }
  }

  def main(args: Array[String]) = {
    println(maxArea(Array(1, 8, 6, 2, 5, 4, 8, 3, 7)))
    println(maxArea(Array(1, 1)))
    println(maxArea(Array(1, 3, 2, 5, 25, 24, 5)))
  }
}
