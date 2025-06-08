package p1

object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val map = nums.zipWithIndex.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    val (n, is) = map.find { case (n, is) =>
      map.contains(target - n) && (target - n != n || is.size > 1)
    }.get
    if (target - n == n) then is else Array(is.head, map(target - n).head)
  }
}
