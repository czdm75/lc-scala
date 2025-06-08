package p228

object Solution {
  def summaryRanges(nums: Array[Int]): List[String] = {
    nums
      .foldLeft(List.empty[(Int, Int)]) {
        case ((from, to) :: tl, n) if n == to + 1 => (from, n) :: tl
        case (list, n)                            => (n, n) :: list
      }
      .reverse
      .map {
        case (x, y) if x == y => x.toString
        case (x, y)           => s"$x->$y"
      }
      .toList
  }
}
