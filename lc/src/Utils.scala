object Utils {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    @tailrec
    def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
        case Some(next) => tailrecM(next)(f)
        case None => ()
    }
}