package p242

object Solution {
  def isAnagram(s: String, t: String): Boolean = {
    s.groupBy(identity).view.mapValues(_.size).toMap == t.groupBy(identity).view.mapValues(_.size).toMap
  }
}
