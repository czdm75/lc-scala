package p20

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def isValid(s: String): Boolean = {
    val matches = Set(('(', ')'), ('[', ']'), ('{', '}'))
    tailrecM((0, List.empty[Char])) {
      case (idx, Nil) if idx == s.length                    => Right(true)
      case (idx, _) if idx == s.length                      => Right(false)
      case (idx, ls) if "([{" contains s(idx)               => Left((idx + 1, s(idx) :: ls))
      case (idx, hd :: tl) if matches contains (hd, s(idx)) => Left((idx + 1, tl))
      case _                                                => Right(false)
    }
  }
}
