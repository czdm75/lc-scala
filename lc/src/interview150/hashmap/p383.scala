package p383

object Solution {
  def canConstruct(ransomNote: String, magazine: String): Boolean = {
    val map = magazine.view.groupBy(identity).view.mapValues(_.size)
    ransomNote.view.groupBy(identity).view.mapValues(_.size).forall { case (c, i) =>
      map.contains(c) && i <= map(c)
    }
  }
}
