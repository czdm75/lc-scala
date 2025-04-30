package p27

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
        case Left(next) => tailrecM(next)(f)
        case Right(b) => b
    }

    def removeElement(nums: Array[Int], `val`: Int): Int = {
        tailrecM((0, nums.length)) { case (start, end) =>
            println(s"start=$start, end=$end")
            if (start == end) {
                Right(start)
            } else if (nums(start) == `val`) {
                // ! Op with side effect: modify array
                nums(start) = nums(end-1)
                Left((start, end-1))
            } else {
                Left((start + 1, end))
            }
        }
    }

    def main(args: Array[String]) = {
        val arr1 = Array(3,2,2,3)
        println(removeElement(arr1, 3))
        println(arr1.mkString(" "))

        val arr2 = Array(0,1,2,2,3,0,4,2)
        println(removeElement(arr2, 2))
        println(arr2.mkString(" "))
    }
}