package p12

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def intToRoman(num: Int): String = {
    val parts = Seq(
      1000 -> "M",
      900 -> "CM",
      500 -> "D",
      400 -> "CD",
      100 -> "C",
      90 -> "XC",
      50 -> "L",
      40 -> "XL",
      10 -> "X",
      9 -> "IX",
      5 -> "V",
      4 -> "IV",
      1 -> "I"
    )

    tailrecM((num, List.empty[String])) {
      case (0, buffer) => Right(buffer.reverse.mkString)
      case (num, buffer) => {
        parts.find(num >= _._1) match
          case Some(n, p) => Left(num - n, p :: buffer)
          case _          => Right(buffer.reverse.mkString)
      }
    }
  }

  def main(args: Array[String]) = {
    println(intToRoman(3749))
    println(intToRoman(58))
    println(intToRoman(1994))
  }
}
