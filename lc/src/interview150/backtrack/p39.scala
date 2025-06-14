package p39

object Solution {
  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    import scala.annotation.tailrec

    @tailrec
    def loop(combinations: List[List[Int]], result: List[List[Int]]): List[List[Int]] = combinations match {
      case Nil                          => result
      case hd :: tl if hd.sum == target => loop(tl, hd :: result)
      case hd :: tl if hd.sum > target  => loop(tl, result)
      case hd :: tl => {
        val newCombs = candidates.view.filter(_ >= hd.head).map(_ :: hd)
        loop(tl prependedAll newCombs, result)
      }
    }

    loop(candidates.map(_ :: Nil).toList, Nil)
  }

  def main(args: Array[String]) = {
    println(combinationSum(Array(2, 3, 6, 7), 7))
    println(combinationSum(Array(2, 3, 5), 8))
    println(combinationSum(Array(2), 1))
  }
}
