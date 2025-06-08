package p172

object Solution {
  def trailingZeroes(n: Int): Int = {
    Iterator.iterate(n / 5)(_ / 5).takeWhile(_ > 0).sum
  }
}
