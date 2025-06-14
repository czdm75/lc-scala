package p148

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  import scala.annotation.tailrec
  def mergeList(l1: ListNode, l2: ListNode): ListNode = {
    @tailrec
    def loop(l1: ListNode, l2: ListNode): ListNode
  }

  def sortList(head: ListNode): ListNode = {
    if (head.next == null) {
      head
    } else {}
  }
}
