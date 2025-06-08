package p17

object Solution {
  def letterCombinations(digits: String): List[String] = {
    val mapping = Map(
      '1' -> "",
      '2' -> "abc",
      '3' -> "def",
      '4' -> "ghi",
      '5' -> "jkl",
      '6' -> "mno",
      '7' -> "pqrs",
      '8' -> "tuv",
      '9' -> "wxyz",
      '0' -> ""
    )

    digits
      .foldLeft(List("")) { case (prefixes, chars) =>
        for {
          p <- prefixes
          c <- mapping(chars)
        } yield p :+ c
      }
      .filterNot(_.isEmpty)
  }
}
