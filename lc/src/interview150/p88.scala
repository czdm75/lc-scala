package p88

object Solution {
    import scala.annotation.tailrec

    @tailrec
    def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
        case Some(next) => tailrecM(next)(f)
        case None => ()
    }

    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
        tailrecM((m-1, n-1)) {
            case (_, -1) => None
            case (-1, p2) =>
                // ! side effect: move element
                nums1(p2) = nums2(p2)
                Some((-1, p2-1))
            case (p1, p2) if nums1(p1) < nums2(p2) =>
                // ! side effect: move element
                nums1(p1 + p2 + 1) = nums2(p2)
                Some((p1, p2-1))
            case (p1, p2) =>
                // ! side effect: move element
                nums1(p1 + p2 + 1) = nums1(p1)
                Some((p1-1, p2))
        }
    }
    
    def main(args: Array[String]) = {
        val a1 = Array(1, 4, 4, 6, 0, 0, 0)
        merge(a1, 4, Array(0, 2, 5), 3)
        println(a1.mkString(" "))
    }
}
