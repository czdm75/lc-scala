package p427

class Node(var _value: Boolean, var _isLeaf: Boolean) {
  var value: Boolean = _value
  var isLeaf: Boolean = _isLeaf
  var topLeft: Node = null
  var topRight: Node = null
  var bottomLeft: Node = null
  var bottomRight: Node = null
}

object Solution {
  def construct(grid: Array[Array[Int]]): Node = {
    def constructSlice(x1: Int, x2: Int, y1: Int, y2: Int): Node = {
      if ((x2 - x1) == 1) {
        Node(grid(x1)(y1) != 0, true)
      } else if ((x2 - x1) == 2) {
        val (a, b, c, d) = (grid(x1)(y1), grid(x1)(y1 + 1), grid(x1 + 1)(y1), grid(x1 + 1)(y1 + 1))
        if (Seq(a, b, c, d).toSet.size == 1) {
          Node(a != 0, true)
        } else {
          val node = Node(true, false)
          node.topLeft = Node(a != 0, true)
          node.topRight = Node(b != 0, true)
          node.bottomLeft = Node(c != 0, true)
          node.bottomRight = Node(d != 0, true)
          node
        }
      } else {
        val a = constructSlice(x1, (x1 + x2) / 2, y1, (y1 + y2) / 2)
        val b = constructSlice(x1, (x1 + x2) / 2, (y1 + y2) / 2, y2)
        val c = constructSlice((x1 + x2) / 2, x2, y1, (y1 + y2) / 2)
        val d = constructSlice((x1 + x2) / 2, x2, (y1 + y2) / 2, y2)
        if (Seq(a, b, c, d).forall(_._isLeaf) && Seq(a, b, c, d).map(_.value).toSet.size == 1) {
          Node(a.value, true)
        } else {
          val node = Node(true, false)
          node.topLeft = a
          node.topRight = b
          node.bottomLeft = c
          node.bottomRight = d
          node
        }
      }
    }
    constructSlice(0, grid.size, 0, grid.size)
  }

  def main(args: Array[String]) = {
    println(construct(Array(Array(0, 1), Array(1, 0))))
  }
}
