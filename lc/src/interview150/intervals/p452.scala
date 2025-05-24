package p452

object Solution {
    def findMinArrowShots(points: Array[Array[Int]]): Int = {
        points.sortBy(_.head).foldLeft(List.empty[(Int, Int)]) {
            case (Nil, Array(x, y)) => (x, y) :: Nil
            case ((a, b) :: tl, Array(x, y)) if x <= b =>
                (math.max(a, x), math.min(b, y)) :: tl
            case (ls, Array(x, y)) => (x, y) :: ls
            case _ => throw new IllegalStateException("")
        }.size
    }

    def main(args: Array[String]) = {
        println(findMinArrowShots(
            Array(10,16,2,8,1,6,7,12).grouped(2).toArray
        ))
        println(findMinArrowShots(
            Array(1,2,3,4,5,6,7,8).grouped(2).toArray
        ))
        println(findMinArrowShots(
            Array(1,2,2,3,3,4,4,5).grouped(2).toArray
        ))
        println(findMinArrowShots(
            Array( 3,9,7,12,3,8,6,8,9,10,2,9,0,9,3,9,0,6,2,8).grouped(2).toArray
        ))
    }
}

