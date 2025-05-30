package p208

final class Trie() {
    import scala.collection.mutable
    import scala.annotation.tailrec

    val map = mutable.Map.empty[Char, Trie]
    var terminate = false

    @tailrec
    def insert(word: String): Unit = {
        if (word.isEmpty) {
            terminate = true
        } else if (map contains word.head) {
            map(word.head).insert(word.tail)
        } else {
            map(word.head) = new Trie()
            map(word.head).insert(word.tail)
        }
    }

    @tailrec
    def search(word: String): Boolean = {
        if (word.isEmpty && terminate) {
            true
        } else if (word.isEmpty && !terminate) {
            false
        } else if (map contains word.head) {
            map(word.head).search(word.tail)
        } else {
            false
        }
    }

    def startsWith(prefix: String): Boolean = {
        if (prefix.isEmpty) {
            true
        } else if (map contains prefix.head) {
            map(prefix.head).startsWith(prefix.tail)
        } else {
            false
        }
    }
}

/**
 * Your Trie object will be instantiated and called as such:
 * val obj = new Trie()
 * obj.insert(word)
 * val param_2 = obj.search(word)
 * val param_3 = obj.startsWith(prefix)
 */