package p2
import environ.ListNode

object Solution {
  import scala.annotation.tailrec

  def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
    case Some(next) => tailrecM(next)(f)
    case None       => ()
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val head = ListNode()
    tailrecM((l1, l2, head, 0)) {
      case (null, null, _, 0) => None
      case (null, null, last, 1) =>
        last.next = new ListNode(1)
        None
      case (l1, l2, last, plus) =>
        val num = Option(l1).map(_.x).getOrElse(0) + Option(l2).map(_.x).getOrElse(0) + plus
        val (digit, newPlus) = (num % 10, num / 10)
        last.next = new ListNode(digit)
        Some((Option(l1).map(_.next).getOrElse(null), Option(l2).map(_.next).getOrElse(null), last.next, newPlus))
    }
    head.next
  }
}
