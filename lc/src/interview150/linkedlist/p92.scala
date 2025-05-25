package p92
import environ.ListNode

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def reverseBetween(head: ListNode, left: Int, right: Int): ListNode = {
    val root = new ListNode(0)
    root.next = head
    
    // left unchanged part. When left == 1, midRoot = root
    val reversedRoot = tailrecM((1, root)) {
      case (idx, last) if idx == left => Right(last)
      case (idx, last) => Left((idx + 1, last.next))
    }
    // reversedTail = toReverseHead
    val reversedTail = reversedRoot.next

    // start reversing
    val rightHead = tailrecM[(Int, ListNode, ListNode), ListNode]((left, null, reversedTail)) {
        // reach end
        case (idx, reversedHead, restHead) if idx == right + 1 => {
            reversedRoot.next = reversedHead
            Right(restHead)
        }
        case (idx, reversedHead, restHead) => {
            val nextRestHead = restHead.next
            val nextReversedHead = restHead
            nextReversedHead.next = reversedHead
            Left((idx + 1, nextReversedHead, nextRestHead))
        }
    }
    reversedTail.next = rightHead
    root.next
  }
}