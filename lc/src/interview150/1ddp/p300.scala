package p300

object Solution {
    def lengthOfLIS(nums: Array[Int]): Int = {
        nums.foldLeft(List((0, Int.MinValue))) { case (lis, n) =>
            println(lis.reverse)
            (lis.filter(_._2 < n).map(_._1 + 1).max, n) :: lis
        }.map(_._1).max
    }

    def main(args: Array[String]) = {
        println(lengthOfLIS(Array(1,2,3,10,8,9,4,5,6)))
        println(lengthOfLIS(Array(1,3,6,7,9,4,10,5,6)))


    }
}