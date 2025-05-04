package p122

object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        (prices.view.tail zip prices.view).map(_ - _).filter(_ > 0).sum
    }

    def main(args: Array[String]) = {
        val arr1 = Array(7,1,5,3,6,4)
        println(maxProfit(arr1))

        val arr2 = Array(1,2,3,4,5)
        println(maxProfit(arr2))

        val arr3 = Array(7,6,4,3,1)
        println(maxProfit(arr3))
    }
}
