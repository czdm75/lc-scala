package p68

object Solution {
  def fullJustify(words: Array[String], maxWidth: Int): List[String] = {
    val (n, lastLine, packed) =
      words.foldLeft[(Int, List[String], List[(Int, List[String])])](
        (0, Nil, Nil)
      ) {
        case ((n, line, packed), word) if (n + line.length + word.length) > maxWidth =>
          (word.length, List(word), (n, line.reverse) :: packed)
        case ((n, line, packed), word) =>
          (n + word.length, word :: line, packed)
      }
    val init = packed.map {
      case (n, word :: Nil) => word + List.fill(maxWidth - n)(' ').mkString
      case (n, words) => {
        val spaces = maxWidth - n
        val (avg, left) =
          (spaces / (words.length - 1), spaces % (words.length - 1))
        words.zipWithIndex.map { case (word, idx) =>
          if (idx < left) {
            word + List.fill(avg + 1)(' ').mkString
          } else if (idx < words.length - 1) {
            word + List.fill(avg)(' ').mkString
          } else {
            word
          }
        }.mkString
      }
    }
    val lastHead = lastLine.reverse.mkString(" ")
    val lastTail = List.fill(maxWidth - lastHead.length)(' ').mkString
    ((lastHead + lastTail) :: init).reverse
  }

  def main(args: Array[String]) = {
    println(
      fullJustify(
        Array("This", "is", "an", "example", "of", "text", "justification."),
        16
      ).mkString("\n")
    )
    println(
      fullJustify(
        Array("What", "must", "be", "acknowledgment", "shall", "be"),
        16
      ).mkString("\n")
    )
    println(
      fullJustify(
        Array(
          "Science",
          "is",
          "what",
          "we",
          "understand",
          "well",
          "enough",
          "to",
          "explain",
          "to",
          "a",
          "computer.",
          "Art",
          "is",
          "everything",
          "else",
          "we",
          "do"
        ),
        20
      ).mkString("\n")
    )
  }
}
