package p26

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    def removeDuplicates(nums: Array[Int]): Int = {
        tailrecM((1, 1)) {
            case (target, idx) if idx == nums.length => Right(target)
            case (target, idx) if nums(idx) == nums(idx - 1) => Left((target, idx+1))
            case (target, idx) =>
                nums(target) = nums(idx)
                Left((target+1, idx+1))
        }
    }

    def main(args: Array[String]) = {
        val arr1 = Array(1,1,2)
        println(removeDuplicates(arr1))
        println(arr1.mkString(" "))

        val arr2 = Array(0,0,1,1,1,2,2,3,3,4)
        println(removeDuplicates(arr2))
        println(arr2.mkString(" "))
    }
}