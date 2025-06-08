package p57

object Solution {
  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    val smallerCnt = intervals.view.takeWhile { case Array(x, y) => y < newInterval.head }.size
    val largerCnt = intervals.view.reverse.takeWhile { case Array(x, y) => x > newInterval.last }.size

    val inserted = if (smallerCnt + largerCnt == intervals.size) {
      // no overlap
      newInterval
    } else {
      val from = math.min(newInterval.head, intervals(smallerCnt).head)
      val to = math.max(newInterval.last, intervals(intervals.size - largerCnt - 1).last)
      Array(from, to)
    }
    intervals.slice(0, smallerCnt) ++ Array(inserted) ++ intervals.slice(intervals.size - largerCnt, intervals.size)
  }

  def main(args: Array[String]) = {
    println(
      insert(
        Array(Array(1, 3), Array(6, 9)),
        Array(2, 5)
      ).map(_.mkString("->")).mkString(" ")
    )
    println(
      insert(
        Array(1, 2, 3, 5, 6, 7, 8, 10, 12, 16).grouped(2).map(_.toArray).toArray,
        Array(4, 8)
      ).map(_.mkString("->")).mkString(" ")
    )
  }
}
