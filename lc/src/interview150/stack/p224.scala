package p224

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def parse(s: String): List[Either[Char, Int]] = {
    s.foldLeft(List.empty[Either[Char, String]]) {
      case (ls, ' ')                                         => ls
      case (ls, c) if "+-()" contains c                      => Left(c) :: ls
      case (Right(hd) :: tl, c) if ("1234567890" contains c) => Right(hd + c) :: tl
      case (ls, c)                                           => Right(c.toString) :: ls
    }.map {
      case Left(c)  => Left(c)
      case Right(s) => Right(s.toInt)
    }.reverse
  }

  def calculate(s: String): Int = {
    tailrecM((List.empty[Either[Char, Int]], parse(s))) {
      // end of loop
      case (Right(n) :: Nil, Nil) => Right(n)
      // reduce stack
      case (Left(')') :: Right(n) :: Left('(') :: stack, input)  => Left((Right(n) :: stack, input))
      case (Right(n2) :: Left('+') :: Right(n1) :: stack, input) => Left((Right(n1 + n2) :: stack, input))
      case (Right(n2) :: Left('-') :: Right(n1) :: stack, input) => Left((Right(n1 - n2) :: stack, input))
      case (Right(n) :: Left('-') :: stack, input)               => Left((Right(-n) :: stack, input))
      // move in
      case (stack, Left(op) :: tl) => Left(Left(op) :: stack, tl)
      case (stack, Right(n) :: tl) => Left(Right(n) :: stack, tl)
      // illegal
      case (stack, input)          => throw new IllegalStateException()
    }
  }

  def main(args: Array[String]) = {
    println(calculate("-1 + 1"))
    println(calculate(" 2-1 + 2 "))
    println(calculate("(1+(4+15+2)-3)+(6+8)"))
    println(calculate("1-(     -2)"))
    println(calculate("- (3 + (4 + 5))"))
  }
}
