package p146

class LRUNode(var prev: LRUNode, var next: LRUNode, val key: Int, var value: Int)

class LRUCache(_capacity: Int) {
  import scala.collection.mutable
  val map = mutable.Map.empty[Int, LRUNode]
  var (first, last) = {
    val nodes = (0 until _capacity).map(_ => new LRUNode(null, null, 0, 0))
    (nodes zip nodes.tail) foreach { case (left, right) =>
      left.next = right
      right.prev = left
    }
    (nodes.head, nodes.last)
  }

  def get(key: Int): Int = {
    if (map contains key) {
      val node = map(key)
      if (node == first) {
        ()
      } else if (node == last) {
        // modify last
        last.prev.next = null
        last = last.prev
        // modify node
        node.prev = null
        node.next = first
        // modify first
        first.prev = node
        first = node
      } else {
        // modify neighbors
        val (left, right) = (node.prev, node.next)
        left.next = right
        right.prev = left
        // modify node
        node.prev = null
        node.next = first
        // modify first
        first.prev = node
        first = node
      }
      node.value
    } else -1
  }

  def put(key: Int, value: Int): Unit = {
    if (map contains key) {
      get(key) // make it at first
      map(key).value = value
    } else {
      // create node
      val node = new LRUNode(null, first, key, value)
      // modify map
      if (map.size == _capacity) {
        // already full
        map.remove(last.key)
      }
      map(key) = node
      // modify list
      first.prev = node
      first = node
      last = last.prev
      last.next = null
    }
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
    val lru = new LRUCache(2)
    println(lru.put(1, 1))
    println(lru.put(2, 2))
    println(lru.get(1))
    println(lru.put(3, 3))
    println(lru.get(2))
    println(lru.put(4, 4))
    println(lru.get(1))
    println(lru.get(3))
    println(lru.get(4))
  }
}

/** Your LRUCache object will be instantiated and called as such: val obj = new LRUCache(capacity) val param_1 =
  * obj.get(key) obj.put(key,value)
  */
