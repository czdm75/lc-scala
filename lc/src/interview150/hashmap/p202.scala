package p202

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    def isHappy(n: Int): Boolean = {
        tailrecM((n, Set.empty[Int])) {
            case (1, _) => Right(true)
            case (n, set) if set contains n => Right(false)
            case (n, set) => Left((n.toString.map(_.toString.toInt).map(i => i * i).sum, set + n))
        }
    }
}
