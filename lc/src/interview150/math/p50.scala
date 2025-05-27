package p50

object Solution {
    def myPow(x: Double, n: Int): Double = {
        if (n == 0) {
            1.0
        } else if (n % 2 == 0) {
            val a = myPow(x, n / 2)
            a * a
        } else if (n < 0) {
            myPow(x, n + 1) / x
        } else {
            myPow(x, n - 1) * x
        }
    }
}