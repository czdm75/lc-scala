package p380

class RandomizedSet() {
  import scala.util.Random
  import scala.collection.mutable

  val map = mutable.Map.empty[Int, Int] // elem -> index
  val idx = mutable.ArrayBuffer.empty[Int]

  def insert(`val`: Int): Boolean = {
    if (map.contains(`val`)) {
      false
    } else {
      map(`val`) = idx.length
      idx.append(`val`)
      true
    }
  }

  def remove(`val`: Int): Boolean = {
    if (map.contains(`val`) && idx.size == 1) {
      map.clear()
      idx.clear()
      true
    } else if (map.contains(`val`) && map(`val`) == idx.size - 1) {
      map.remove(`val`)
      idx.remove(idx.length - 1)
      true
    } else if (map.contains(`val`)) {
      val removeIdx = map(`val`)
      map.remove(`val`)
      map(idx.last) = removeIdx
      idx(removeIdx) = idx.last
      idx.remove(idx.length - 1)
      true
    } else {
      false
    }
  }

  def getRandom(): Int = idx(Random.nextInt(idx.length))
}

object Solution {
  def main(args: Array[String]) = {
    val set = RandomizedSet()
    set.insert(0)
    set.insert(1)
    set.remove(0)
    set.insert(2)
    set.remove(1)
    println(set.getRandom())
  }
}
