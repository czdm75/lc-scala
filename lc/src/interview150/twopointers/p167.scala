package p167

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    tailrecM((0, numbers.length - 1)) {
      case (p1, p2) if numbers(p1) + numbers(p2) > target => Left((p1, p2 - 1))
      case (p1, p2) if numbers(p1) + numbers(p2) < target => Left((p1 + 1, p2))
      case (p1, p2) => Right(Array(p1 + 1, p2 + 1))
    }
  }

  def main(args: Array[String]) = {
    println(twoSum(Array(2, 7, 11, 15), target = 9).mkString(" "))
    println(twoSum(Array(2, 3, 4), target = 6).mkString(" "))
    println(twoSum(Array(-1, 0), target = -1).mkString(" "))
  }
}
