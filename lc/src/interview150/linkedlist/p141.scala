package p141
import environ.ListNode

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def hasCycle(head: ListNode): Boolean = {
    if (head == null) {
      false
    } else {
      tailrecM((head.next, head)) {
        case (null, slow)                      => Right(false)
        case (fast, slow) if fast eq slow      => Right(true)
        case (fast, slow) if fast.next == null => Right(false)
        case (fast, slow)                      => Left((fast.next.next, slow.next))
      }
    }
  }
}
