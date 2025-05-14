package p6

object Solution {
  def convert(s: String, numRows: Int): String = {
    // for a numsRows = n, pattern has size (2 * n - 2), except 1
    val patternSize = math.max(2 * numRows - 2, 1)
    (0 until numRows)
      .flatMap {
        case 0 => (0 until s.length by patternSize)
        case n if n == numRows - 1 => (
          numRows - 1 until s.length by patternSize
        )
        case n =>
          s.indices.filter { i =>
            i % patternSize == n || (i % patternSize + n == patternSize)
          }.toSeq
      }
      .map(s)
      .mkString
  }

  def main(args: Array[String]) = {
    println(convert("PAYPALISHIRING", 3))
    println(convert("PAYPALISHIRING", 4))
    println(convert("A", 1))
  }
}
