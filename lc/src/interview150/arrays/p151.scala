package p151

object Solution {
  def reverseWords(s: String): String = {
    val (charBuffer, stringBuffer) =
      s.foldLeft[(Option[List[Char]], List[String])]((None, Nil)) {
        case ((None, stringBuffer), ' ') =>
          (None, stringBuffer) // continue spaces
        case ((None, stringBuffer), char) =>
          (Some(List(char)), stringBuffer) // start of a word
        case ((Some(charBuffer), stringBuffer), ' ') =>
          (None, charBuffer.reverse.mkString :: stringBuffer) // end of word
        case ((Some(charBuffer), stringBuffer), char) =>
          (Some(char :: charBuffer), stringBuffer) // continue word
      }
    // finish last loop
    charBuffer
      .map(_.reverse.mkString)
      .map(_ :: stringBuffer)
      .getOrElse(stringBuffer)
      .mkString(" ")
  }

  def main(args: Array[String]) = {
    println(reverseWords("the sky is blue"))
    println(reverseWords("  hello world  "))
    println(reverseWords("a good   example"))
  }
}
