package p373

object Solution {
  def kSmallestPairs(nums1: Array[Int], nums2: Array[Int], k: Int): List[List[Int]] = {
    import scala.collection.mutable
    val ordering = { (x: Int, y: Int) => nums1(x) + nums2(y) }.tupled
    val minHeap = mutable.PriorityQueue((0, 0))(Ordering.by(ordering).reverse)
    val minSet = mutable.Set((0, 0))
    (0 until k).map { _ =>
      val (x, y) = minHeap.dequeue()
      if (y + 1 < nums2.size && !minSet.contains((x, y + 1))) {
        minHeap.enqueue((x, y + 1))
        minSet.add((x, y + 1))
      }
      if (x + 1 < nums1.size && !minSet.contains((x + 1, y))) {
        minHeap.enqueue((x + 1, y))
        minSet.add((x + 1, y))
      }
      List(nums1(x), nums2(y))
    }.toList
  }

  def main(args: Array[String]) = {
    println(kSmallestPairs(Array(1, 7, 11), Array(2, 4, 6), 3))
    println(kSmallestPairs(Array(1, 2, 4, 5, 6), Array(3, 5, 7, 9), 20))
  }
}
