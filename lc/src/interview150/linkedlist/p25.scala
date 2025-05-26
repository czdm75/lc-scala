package p25
import environ.ListNode
import javax.management.ListenerNotFoundException

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def reverseSingleGroup(head: ListNode, k: Int): (ListNode, ListNode) = {
    tailrecM((1, head, head.next)) {
      case (idx, reversedHead, restHead) if idx == k => {
        head.next = restHead
        Right((reversedHead, head))
      }
      case (idx, reversedHead, restHead) => {
        val newRestHead = restHead.next
        val newRevHead = restHead
        newRevHead.next = reversedHead
        Left((idx + 1, newRevHead, newRestHead))
      }
    }
  }

  def checkReverseGroup(head: ListNode, k: Int): Boolean = {
    tailrecM((1, head)) {
      case (_, null)        => Right(false)
      case (i, _) if i == k => Right(true)
      case (i, l)           => Left((i + 1, l.next))
    }
  }

  def reverseKGroup(head: ListNode, k: Int): ListNode = {
    val root = ListNode(0, head)

    tailrecM(root) { last =>
      if (last != null && checkReverseGroup(last.next, k)) {
        val (reversedHead, reversedLast) = reverseSingleGroup(last.next, k)
        last.next = reversedHead
        Left(reversedLast)
      } else {
        Right(())
      }
    }

    root.next
  }

  def main(args: Array[String]) = {
    val ll = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
    println(reverseKGroup(ll, 2))
    val lll = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
    println(reverseKGroup(lll, 3))
  }
}
