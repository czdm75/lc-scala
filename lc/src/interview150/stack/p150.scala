package p150

object Solution {
    def evalRPN(tokens: Array[String]): Int = {
        tokens.foldLeft(List.empty[Int]) {
            case (h1 :: h2 :: tl, "+") => h2 + h1 :: tl
            case (h1 :: h2 :: tl, "-") => h2 - h1 :: tl
            case (h1 :: h2 :: tl, "*") => h2 * h1 :: tl
            case (h1 :: h2 :: tl, "/") => h2 / h1 :: tl
            case (ls, num) => num.toInt :: ls
        }.head
    }
}