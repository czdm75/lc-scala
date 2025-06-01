package p46

object Solution {
  def permute(nums: Array[Int]): List[List[Int]] = {
    nums.indices.foldLeft(List(List.empty[Int])) { case (prefixes, _) =>
      for {
        p <- prefixes
        x <- nums if !p.contains(x)
      } yield x :: p
    }
  }

  def main(args: Array[String]) = {
    println(permute(Array(1, 2, 3)))
  }
}
