package p139

object Solution {
    def wordBreak(s: String, wordDict: List[String]): Boolean = {
        s.foldLeft(List("")) { case (suffixes, char) =>
            println(suffixes)
            val newSuffixes = suffixes.map(_ + char.toString)
            if (newSuffixes.exists(wordDict.contains)) {
                "" :: newSuffixes
            } else {
                newSuffixes
            }
        }.exists(_.isEmpty)
    }

    def main(args: Array[String]) = {
        println(wordBreak("leetcode", "leet" :: "code" :: Nil))
    }
}