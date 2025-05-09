package p45

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    def jump(nums: Array[Int]): Int = {
        tailrecM((0, 0, 0)) {
            case (step, start, end) if end >= nums.length - 1 => Right(step)
            case (step, start, end) =>
                val newEnd = (start to end).map { i =>
                    i + nums(i)
                }.max
                Left((step + 1, start + 1, newEnd))
        }
    }

    def main(args: Array[String]) = {
        println(jump(Array(2,3,1,1,4)))
        println(jump(Array(2,3,0,1,4)))
        println(jump(Array(0)))
    }
}
