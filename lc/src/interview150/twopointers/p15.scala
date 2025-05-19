package p15

object Solution {
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val m = nums.view.groupBy(identity).mapValues(_.toSeq.length).toMap
    val keys = m.keys.toIndexedSeq
    val result = for {
      i1 <- (0 until keys.length)
      v1 = keys(i1)
      i2 <- (i1 until keys.length)
      v2 = keys(i2)
      v3 = 0 - keys(i1) - keys(i2)
      if m.contains(v3)
      // distinct, then check count
      if Seq(v1, v2, v3).groupBy(identity).forall((n, ns) => m(n) >= ns.length)
    } yield List(v1, v2, v3).sorted
    result.distinct.toList
  }

  def main(args: Array[String]) = {
    println(
      threeSum(Array(-1, 0, 1, 2, -1, -4)).map(_.mkString(",")).mkString(" ")
    )
    println(threeSum(Array(0, 1, 1)).map(_.mkString(",")).mkString(" "))
    println(threeSum(Array(0, 0, 0)).map(_.mkString(",")).mkString(" "))
    println(threeSum(Array(-1, 0, 1)).map(_.mkString(",")).mkString(" "))
    println(
      threeSum(
        Array(2, -3, 0, -2, -5, -5, -4, 1, 2, -2, 2, 0, 2, -4, 5, 5, -10)
      ).map(_.mkString(",")).mkString("   ")
    )
  }
}
