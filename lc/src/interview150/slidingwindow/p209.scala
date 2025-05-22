package p209

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def minSubArrayLen(target: Int, nums: Array[Int]): Int = {
    val sum = nums.sum
    if (sum < target) {
      0
    } else {
      tailrecM((0, nums.length, sum, nums.length)) {
        // found result for this iteration
        case (start, end, sum, minWidth) if sum >= target && sum - nums(end - 1) < target =>
          Left((start + 1, end, sum - nums(start), math.min(minWidth, end - start)))
        // search left
        case (start, end, sum, minWidth) if sum >= target =>
          Left((start, end - 1, sum - nums(end - 1), minWidth))
        // reach end but still not enough
        case (start, end, sum, minWidth) if sum < target && end == nums.length =>
          Right(minWidth)
        // search right
        case (start, end, sum, minWidth) =>
          Left((start, end + 1, sum + nums(end), minWidth))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(minSubArrayLen(target = 7, nums = Array(2, 3, 1, 2, 4, 3)))
    println(minSubArrayLen(target = 4, nums = Array(1, 4, 4)))
    println(minSubArrayLen(target = 11, nums = Array(1, 1, 1, 1, 1, 1, 1, 1)))
  }
}
