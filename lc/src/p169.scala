package p169

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    def majorityElement(nums: Array[Int]): Int = {
        tailrecM((nums.head, 1, nums.indices.tail)) { case (maxNum, maxCnt, indices) =>
            println(s"maxNum=$maxNum, maxCnt = $maxCnt, indices=$indices")
            // end of recursion
            if (indices.isEmpty || maxCnt > indices.last / 2) {
                Right(maxNum)
            } else if (maxNum == nums(indices.head)) {
                Left((maxNum, maxCnt + 1, indices.tail))
            } else if (maxCnt == 0) {
                Left((nums(indices.head), 1, indices.tail))
            } else {
                Left((maxNum, maxCnt - 1, indices.tail))
            }
        }
    }
}
