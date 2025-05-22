package p3

object Solution {
    def lengthOfLongestSubstring(s: String): Int = {
        val (max, map) = s.zipWithIndex.foldLeft((0, Map.empty[Char, Int])) {
            case ((max, map), (c, i)) if map.contains(c) =>
                val newMax = math.max(max, map.size)
                val newMap = map.filter(_._2 > map(c)) + (c -> i)
                (newMax, newMap)
            case ((max, map), (c, i)) =>
                (max, map + (c -> i))
        }
        math.max(max, map.size)
    }

    def main(args: Array[String]) = {
        println(lengthOfLongestSubstring("abcabcbb"))
        println(lengthOfLongestSubstring("bbbbb"))
        println(lengthOfLongestSubstring("pwwkew"))
    }
}
