package p70

object Solution {
    def climbStairs(n: Int): Int = {
        (0 until n).foldLeft((0, 1)) { case ((last2, last), n) =>
            (last, last2 + last)
        }._2
    }
}