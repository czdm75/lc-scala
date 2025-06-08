package p49

object Solution {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    strs.groupBy(_.groupBy(identity).view.mapValues(_.size).toMap).values.map(_.toList).toList
  }
}
