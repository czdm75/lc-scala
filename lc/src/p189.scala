package p189

object Solution {
    def rotate(nums: Array[Int], k: Int): Unit = {
        val kk = if k > nums.length then k % nums.length else k
        val tail = nums.slice(nums.length - kk, nums.length)
        (0 until nums.length - kk).reverse.foreach { i => nums(i+kk) = nums(i) }
        tail.copyToArray(nums)
    }

    def main(args: Array[String]) = {
        val arr1 = Array(1,2,3,4,5,6,7)
        rotate(arr1, 3)
        println(arr1.mkString(" "))

        val arr2 = Array(-1,-100,3,99)
        rotate(arr2, 2)
        println(arr2.mkString(" "))

        val arr3 = Array(1, 2)
        rotate(arr3, 3)
        println(arr3.mkString(" "))
    }
}