package p128

object Solution {
    def longestConsecutive(nums: Array[Int]): Int = {
        val (startMap, endMap) = nums.distinct.foldLeft((Map.empty[Int, Int], Map.empty[Int, Int])) { case ((startMap, endMap), n) => 
            (startMap contains (n+1), endMap contains (n-1)) match {
                case (false, false) => (startMap + (n -> n), endMap + (n -> n))
                case (true,  false) =>
                    val seqEnd = startMap(n+1)
                    (startMap - (n+1) + (n -> seqEnd), endMap + (seqEnd -> n))
                case (false, true) =>
                    val seqStart = endMap(n-1)
                    (startMap + (seqStart -> n), endMap - (n-1) + (n -> seqStart))
                case (true,  true ) =>
                    val seqEnd = startMap(n+1)
                    val seqStart = endMap(n-1)
                    (startMap - (n+1) + (seqStart -> seqEnd), endMap - (n-1) + (seqEnd -> seqStart))
            }
        }
        if endMap.isEmpty then 0 else endMap.map(_ - _).max + 1
    }

    def main(args: Array[String]) = {
        println(longestConsecutive(Array(100,4,200,1,3,2)))
        println(longestConsecutive(Array(0,3,7,2,5,8,4,6,0,1)))
        println(longestConsecutive(Array(1,0,1,2)))
        println(longestConsecutive(Array()))
    }
}