package utils

import scala.collection.mutable.ArrayBuffer

object Utils {
  import scala.annotation.tailrec

  @tailrec
  def tailrecM[A, B](init: A)(f: A => Either[A, B]): B = f(init) match {
    case Left(next) => tailrecM(next)(f)
    case Right(b)   => b
  }

  @tailrec
  def tailrecM[A](init: A)(f: A => Option[A]): Unit = f(init) match {
    case Some(next) => tailrecM(next)(f)
    case None       => ()
  }
}

final class Heap {
  import scala.annotation.tailrec
  import scala.collection.mutable.ArrayBuffer

  val heap = ArrayBuffer.empty[Int]
  var size = 0

  def leftChild(n: Int) = 2 * n + 1
  def rightChild(n: Int) = 2 * n + 2
  def parent(n: Int): Int = (n - 1) / 2

  def top: Int = heap.head

  def swap(n1: Int, n2: Int): Unit = {
    val tmp = heap(n1)
    heap(n1) = heap(n2)
    heap(n2) = tmp
  }

  @tailrec
  def sink(n: Int = 0): Unit = {
    if (rightChild(n) < size && heap(rightChild(n)) > heap(n) && heap(rightChild(n)) >= heap(leftChild(n))) {
      // have two child, right > heap && right > left
      swap(rightChild(n), n)
      sink(rightChild(n))
    } else if (leftChild(n) < size && heap(leftChild(n)) > heap(n)) {
      // have one or two child, left > heap
      swap(leftChild(n), n)
      sink(leftChild(n))
    }
  }

  def pop(): Int = {
    val result = top
    heap(0) = heap(size - 1)
    size -= 1
    sink()
    result
  }

  @tailrec
  def float(n: Int = size - 1): Unit = {
    if (parent(n) >= 0 && heap(n) > heap(parent(n))) {
      swap(parent(n), n)
      float(parent(n))
    }
  }

  def push(x: Int): Unit = {
    if (size == heap.size) {
      heap.append(x)
    } else {
      heap(size) = x
    }
    size += 1
    float()
  }
}

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