package p21
import environ.ListNode

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
    case Some(next) => tailrecM(next)(f)
    case None       => ()
  }

  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
    val head = new ListNode()
    tailrecM((list1, list2, head)) {
      case (null, null, _) => None
      case (l1, null, last) =>
        last.next = l1
        Some((l1.next, null, last.next))
      case (null, l2, last) =>
        last.next = l2
        Some((l2.next, null, last.next))
      case (l1, l2, last) if l1.x <= l2.x =>
        last.next = l1
        Some((l1.next, l2, last.next))
      case (l1, l2, last) =>
        last.next = l2
        Some((l1, l2.next, last.next))
    }
    head.next
  }
}
