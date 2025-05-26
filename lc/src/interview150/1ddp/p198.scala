package p198

object Solution {
  def rob(nums: Array[Int]): Int = {
    (math.max _).tupled(nums.foldLeft((0, 0)) { case ((lastRobMax, lastNotRobMax), current) =>
      val thisRobMax = lastNotRobMax + current
      val thisNotRobMax = math.max(lastRobMax, lastNotRobMax)
      (thisRobMax, thisNotRobMax)
    })
  }
}
