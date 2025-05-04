package p121

object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        prices.foldLeft((Int.MaxValue, 0)) {
            case ((prevMin, maxProfit), price) =>
                val newMin = math.min(prevMin, price)
                val newProfit = math.max(math.max(price - prevMin, 0), maxProfit)
                (newMin, newProfit)
        }._2
    }

    def main(args: Array[String]) = {
        val arr1 = Array(7,1,5,3,6,4)
        println(maxProfit(arr1))

        val arr2 = Array(7,6,4,3,1)
        println(maxProfit(arr2))
    }
}
