package p215

import utils.Heap

object Solution {
  def findKthLargest(nums: Array[Int], k: Int): Int = {
    val heap = new Heap()
    nums.foreach(heap.push)
    println(heap.heap.mkString(" "))
    (1 until k).foreach { _ =>
      heap.pop(); println(heap.heap.take(heap.size).mkString(" "))
    }
    println(heap.heap.take(heap.size).mkString(" "))
    heap.top
  }

  def main(args: Array[String]) = {
    println(findKthLargest(Array(3, 2, 1, 5, 6, 4), 2))
    println(findKthLargest(Array(7, 6, 5, 4, 3, 2, 1), 5))
  }
}
