package p148

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  import scala.annotation.tailrec
  @tailrec
  def mergeList(l1: ListNode, l2: ListNode, last: ListNode): Unit = {
    if (l1 == null && l2 == null) {
      last.next = null
    } else if (l1 == null || (l2 != null && l1.x > l2.x)) {
      last.next = l2
      mergeList(l1, l2.next, l2)
    } else {
      last.next = l1
      mergeList(l1.next, l2, l1)
    }
  }

  @tailrec
  def listMid(fast: ListNode, slow: ListNode): ListNode = {
    if (fast.next == null || fast.next.next == null) {
      slow
    } else {
      listMid(fast.next.next, slow.next)
    }
  }

  def sortList(head: ListNode): ListNode = {
    if (head == null || head.next == null) {
      head
    } else {
      val mid = listMid(head, head)
      // detatch
      val tmp = mid.next
      mid.next = null

      val left = sortList(head)
      val right = sortList(tmp)

      val root = ListNode()
      mergeList(left, right, root)
      root.next
    }
  }

  def main(args: Array[String]) = {
    val list = ListNode(4, ListNode(2, ListNode(1, ListNode(3))))
    
    println(Iterator.iterate(sortList(list))(_.next).takeWhile(_ != null).map(_.x).mkString(" "))
  }
}
