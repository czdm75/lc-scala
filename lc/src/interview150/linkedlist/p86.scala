package p86
import environ.ListNode

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def partition(head: ListNode, x: Int): ListNode = {
    val (smallerRoot, largerRoot) = (new ListNode(), new ListNode())
    tailrecM((smallerRoot, largerRoot, head)) {
      case (smallerLast, largerLast, current) => {
        if (current == null) {
          smallerLast.next = largerRoot.next
          largerLast.next = null
          Right(smallerRoot.next)
        } else if (current.x < x) {
          smallerLast.next = current
          Left((current, largerLast, current.next))
        } else {
          largerLast.next = current
          Left((smallerLast, current, current.next))
        }
      }
    }
  }

  def main(args: Array[String]) = {
    val ll = ListNode(1, ListNode(4, ListNode(3, ListNode(2, ListNode(5, ListNode(2))))))
    val head = partition(ll, 3)
    println(Iterator.iterate(head)(_.next).takeWhile(_ != null).map(_.x).mkString(" "))
  }
}
