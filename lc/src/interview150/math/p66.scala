package p66

object Solution {
  def plusOne(digits: Array[Int]): Array[Int] = {
    val (ds, add1) = digits.reverseIterator.foldLeft(List.empty[Int], true) {
      case ((ls, false), n) => (n :: ls, false)
      case ((ls, true), 9)  => (0 :: ls, true)
      case ((ls, true), n)  => (n + 1 :: ls, false)
    }
    (if add1 then 1 :: ds else ds).toArray
  }
}
