package p30

object Solution {
    def findSubstring(s: String, words: Array[String]): List[Int] = {
        val map = words.groupBy(identity).view.mapValues(_.length).toMap
        val (m, n) = (words.head.length, words.length)
        (0 to s.length - m * n).filter { i =>
            Range(i, i + m * n, m)
                .map(start => s.substring(start, start + m))
                .groupBy(identity)
                .view.mapValues(_.length).toMap == map
        }.toList
    }

    def main(args: Array[String]) = {
        println(findSubstring(s = "barfoothefoobarman", words = Array("foo","bar")).mkString(","))
        println(findSubstring(s = "wordgoodgoodgoodbestword", words = Array("word","good","best","word")).mkString(","))
        println(findSubstring(s = "barfoofoobarthefoobarman", words = Array("bar","foo","the")).mkString(","))
        println(findSubstring(s = "wordgoodgoodgoodbestword", words = Array("word","good","best","good")).mkString(","))
    }
}