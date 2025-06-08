package p138

class Node(var _value: Int) {
  var value: Int = _value
  var next: Node = null
  var random: Node = null
}

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
    case Some(next) => tailrecM(next)(f)
    case None       => ()
  }

  def copyRandomList(head: Node): Node = {
    val root = Node(0)
    root.next = head
    // create interleaved list
    tailrecM(head) {
      case null => None
      case current =>
        val newNode = new Node(current._value)
        newNode.next = current.next
        current.next = newNode
        Some(newNode.next)
    }
    // update randoms
    tailrecM(head) {
      case null => None
      case current if current.random == null =>
        Some(current.next.next)
      case current =>
        current.next.random = current.random.next
        Some(current.next.next)
    }
    // extract new list
    tailrecM(root) {
      case null                            => None
      case current if current.next == null => None
      case current =>
        val next = current.next.next
        current.next.next = current.next.next.next
        current.next = next
        Some(current.next)
    }
    root.next
  }
}
