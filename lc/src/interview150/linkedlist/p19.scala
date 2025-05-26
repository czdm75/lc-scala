package p19
import environ.ListNode

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
    case Some(next) => tailrecM(next)(f)
    case None       => ()
  }

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    val root = ListNode(0, head)
    val fastRoot = (0 until n).foldLeft(root)((last, _) => last.next)
    tailrecM((fastRoot, root)) { case (fastlast, slowLast) =>
      if (fastlast.next == null) {
        slowLast.next = slowLast.next.next
        None
      } else {
        Some((fastlast.next, slowLast.next))
      }
    }
    root.next
  }
}
