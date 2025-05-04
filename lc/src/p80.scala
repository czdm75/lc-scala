package p80

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    def removeDuplicates(nums: Array[Int]): Int = {
        if (nums.length < 2) {
            return nums.length
        } else {
            tailrecM((2, 2, (nums(0), nums(1)))) {
                case (target, idx, (_, _)) if idx == nums.length => Right(target)
                case (target, idx, (last2, last1)) if nums(idx) == last1 && nums(idx) == last2 =>
                    val lasts = (last1, nums(idx))
                    Left((target, idx+1, lasts))
                case (target, idx, (last2, last1)) =>
                    val lasts = (last1, nums(idx))
                    nums(target) = nums(idx)
                    Left((target+1, idx+1, lasts))
            }
        }
    }

    def main(args: Array[String]) = {
        val arr1 = Array(1,1,1,2,2,3)
        println(removeDuplicates(arr1))
        println(arr1.mkString(" "))

        val arr2 = Array(0,0,1,1,1,1,2,3,3)
        println(removeDuplicates(arr2))
        println(arr2.mkString(" "))
    }
}
