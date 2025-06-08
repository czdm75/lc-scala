package p238

object Solution {
  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    val leftProducts = nums.view.scanLeft(1)(_ * _).init
    val rightProducts = nums.view.scanRight(1)(_ * _).tail
    (leftProducts zip rightProducts).map(_ * _).toArray
  }

  def productExceptSelf2(nums: Array[Int]): Array[Int] = {
    val result = Array.fill(nums.length)(1)
    nums.view.zipWithIndex.foldLeft(1) { case (prod, (elem, idx)) =>
      result(idx) = prod
      prod * elem
    }

    nums.reverseIterator.foldLeft((1, nums.length - 1)) { case ((prod, idx), elem) =>
      result(idx) *= prod
      (prod * elem, idx - 1)
    }

    result
  }

  def main(args: Array[String]) = {
    println(productExceptSelf(Array(1, 2, 3, 4)).mkString(" "))
    println(productExceptSelf(Array(-1, 1, 0, -3, 3)).mkString(" "))

    println(productExceptSelf2(Array(1, 2, 3, 4)).mkString(" "))
    println(productExceptSelf2(Array(-1, 1, 0, -3, 3)).mkString(" "))
  }
}
