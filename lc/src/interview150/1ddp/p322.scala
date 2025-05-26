package p322

object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
        (0 until amount).foldLeft(List(0)) {
            case (minCoins, amt) => {
                val thisCoins = coins.flatMap(i => minCoins.drop(i-1).headOption.filterNot(_ == -1).map(_ + 1))
                (if thisCoins.isEmpty then -1 else thisCoins.min) :: minCoins
            }
        }.head
    }

    def main(args: Array[String]) = {
        println(coinChange(Array(1,2,5), 11))
        println(coinChange(Array(2), 3))
        println(coinChange(Array(1), 0))
    }
}