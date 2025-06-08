package p56

object Solution {
  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    intervals
      .sortBy(_.head)
      .foldLeft(List.empty[Array[Int]]) {
        case (Nil, Array(x, y)) => Array(x, y) :: Nil
        // x always >= from
        case (Array(from, to) :: tl, Array(x, y)) if x > to => Array(x, y) :: Array(from, to) :: tl
        case (Array(from, to) :: tl, Array(x, y))           => Array(from, math.max(to, y)) :: tl
        case _                                              => throw new IllegalStateException("")
      }
      .reverse
      .toArray
  }
}
