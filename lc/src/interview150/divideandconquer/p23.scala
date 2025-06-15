package p23

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def mergeKLists(lists: Array[ListNode]): ListNode = {
    import scala.annotation.tailrec
    @tailrec
    def merge(last: ListNode): Unit = {
      val nodes = lists.zipWithIndex.filter(_._1 != null)
      if (nodes.isEmpty) {
        last.next = null
      } else {
        val idx = nodes.minBy(_._1.x)._2
        last.next = lists(idx)
        lists(idx) = lists(idx).next
        merge(last.next)
      }
    }
    val root = ListNode()
    merge(root)
    root.next
  }
}
