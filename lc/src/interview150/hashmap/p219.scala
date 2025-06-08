package p219

object Solution {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  def containsNearbyDuplicate(nums: Array[Int], k: Int): Boolean = {
    tailrecM((0, Map.empty[Int, Set[Int]])) {
      case (idx, _) if idx == nums.length                                                => Right(false)
      case (idx, map) if map.getOrElse(nums(idx), Set.empty).exists(i => (idx - i) <= k) => Right(true)
      case (idx, map) => Left(idx + 1, map.updatedWith(nums(idx))(opt => Some(opt.getOrElse(Set.empty) + idx)))
    }
  }
}
