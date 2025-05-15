package p13

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def romanToInt(s: String): Int = {
    val value = Map(
      'M' -> 1000,
      'D' -> 500,
      'C' -> 100,
      'L' -> 50,
      'X' -> 10,
      'V' -> 5,
      'I' -> 1
    )
    val n = s.length
    tailrecM((0, 0)) {
      case (idx, sum) if idx == n     => Right(sum)
      case (idx, sum) if idx == n - 1 => Left((n, sum + value(s.last)))
      case (idx, sum) if value(s(idx)) < value(s(idx + 1)) =>
        Left((idx + 2, sum + value(s(idx + 1)) - value(s(idx))))
      case (idx, sum) => Left((idx + 1, sum + value(s(idx))))
    }
  }

  def main(args: Array[String]) = {
    println(romanToInt("III"))
    println(romanToInt("LVIII"))
    println(romanToInt("MCMXCIV"))
  }
}
