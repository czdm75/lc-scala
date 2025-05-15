package p55

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    def canJump(nums: Array[Int]): Boolean = {
        tailrecM((1, nums.head)) {
            case (start, end) if end >= nums.length - 1 => Right(true)
            case (start, end) if start > end => Right(false)
            case (start, end) =>
                val newLast = start + nums(start)
                Left(start+1, math.max(newLast, end))
        }
    }

    def main(args: Array[String]) = {
        println(canJump(Array(2,3,1,1,4)))
        println(canJump(Array(3,2,1,0,4)))
    }
}
