package p82
import environ.ListNode

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
    case Some(next) => tailrecM(next)(f)
    case None       => ()
  }

  def deleteDuplicates(head: ListNode): ListNode = {
    val root = new ListNode(0, head)
    tailrecM(root) { last =>
      if (last.next != null && last.next.next != null && last.next.x == last.next.next.x) {
        val dup = last.next.x
        while (last.next != null && last.next.x == dup) {
          last.next = last.next.next
        }
        Some(last)
      } else if (last.next != null) {
        Some(last.next)
      } else {
        None
      }
    }
    root.next
  }
}
