package p134

object Solution {
  def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
    if (gas.sum < cost.sum) {
      -1
    } else {
      (gas.view zip cost.view).zipWithIndex
        .foldLeft((0, 0)) { case ((start, left), ((g, c), idx)) =>
          val newLeft = left + g - c
          if (newLeft < 0) {
            // failed
            (idx + 1, 0)
          } else {
            (start, newLeft)
          }
        }
        ._1
    }
  }

  def main(args: Array[String]) = {
    println(
      canCompleteCircuit(
        gas = Array(1, 2, 3, 4, 5),
        cost = Array(3, 4, 5, 1, 2)
      )
    )
    println(canCompleteCircuit(gas = Array(2, 3, 4), cost = Array(3, 4, 3)))
    println(
      canCompleteCircuit(
        gas = Array(2, 0, 1, 2, 3, 4, 0),
        cost = Array(0, 1, 0, 0, 0, 0, 11)
      )
    )
  }
}
