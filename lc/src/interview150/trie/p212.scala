package p212

final class Trie() {
  import scala.collection.mutable
  import scala.annotation.tailrec

  val map = mutable.Map.empty[Char, Trie]
  var terminate = false

  @tailrec
  def insert(word: String): Unit = {
    if (word.isEmpty) {
      terminate = true
    } else if (map contains word.head) {
      map(word.head).insert(word.tail)
    } else {
      map(word.head) = new Trie()
      map(word.head).insert(word.tail)
    }
  }

  @tailrec
  private def doSearch(word: String, searchPrefix: Boolean): Boolean = {
    if (word.isEmpty) {
      terminate || searchPrefix
    } else if (map contains word.head) {
      map(word.head).doSearch(word.tail, searchPrefix)
    } else {
      false
    }
  }

  def search(word: String): Boolean = doSearch(word, false)

  def prefix(word: String): Boolean = doSearch(word, true)

  def remove(word: String): Unit = {
    if (word.isEmpty) {
      terminate = false
    } else if (map contains word.head) {
      val subTree = map(word.head)
      subTree.remove(word.tail)
      if (!subTree.terminate && subTree.map.isEmpty) {
        map.remove(word.head)
      }
    } else {
      throw new java.lang.IllegalArgumentException("")
    }
  }
}

object Solution {

  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    val trie = new Trie()
    words.foreach(trie.insert)

    import scala.annotation.tailrec

    @tailrec
    def searchBoard(targets: List[(Int, Int, String, Set[(Int, Int)])], results: List[String]): List[String] =
      targets match {
        case Nil => results
        case (x, y, word, prev) :: tl => {
          val char = board(x)(y)
          val w = word :+ char
          println(s"cord: ($x, $y), search: $w, prevSet: $prev")

          val newResult = if (trie.search(w)) {
            trie.remove(w)
            w :: results
          } else results

          val newTargets = if (trie.prefix(w)) {
            val cordinates = Seq((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
              .filterNot(prev.contains)
              .filter { case (x, y) => x >= 0 && y >= 0 && x < board.size && y < board.head.size }
            val newChecks = cordinates.map { case (i, j) => (i, j, w, prev + ((x, y))) }
            newChecks.toList ++ tl
          } else tl

          searchBoard(newTargets, newResult)
        }
      }

    val res = for {
      x <- board.indices
      y <- board.head.indices
      result <- searchBoard(List((x, y, "", Set.empty)), Nil)
    } yield result
    res.toList
  }

  def main(args: Array[String]) = {
    println(
      findWords(
        Array('o', 'a', 'a', 'n', 'e', 't', 'a', 'e', 'i', 'h', 'k', 'r', 'i', 'f', 'l', 'v').grouped(4).toArray,
        Array("oath", "pea", "eat", "rain")
      )
    )
    println(findWords(Array('a', 'a').grouped(2).toArray, Array("aaa")))

  }
}
