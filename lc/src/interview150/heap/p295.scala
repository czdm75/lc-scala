package p295

class MedianFinder() {
    import scala.collection.mutable
    val largeHeap = mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
    val smallHeap = mutable.PriorityQueue.empty[Int]

    def addNum(num: Int): Unit = {
        if (largeHeap.isEmpty || num > largeHeap.head) {
            largeHeap.enqueue(num)
        } else {
            smallHeap.enqueue(num)
        } 
    } 

    def findMedian(): Double = {
        // balance
        while (largeHeap.size > smallHeap.size) {
            smallHeap.enqueue(largeHeap.dequeue())
        }
        // largeHeap is either smaller or same
        while (largeHeap.size < smallHeap.size) {
            largeHeap.enqueue(smallHeap.dequeue())
        }
        // largeHeap is either 1 larger or same
        if (largeHeap.size == smallHeap.size) {
            (largeHeap.head + smallHeap.head) / 2.0
        } else {
            largeHeap.head
        }
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * val obj = new MedianFinder()
 * obj.addNum(num)
 * val param_2 = obj.findMedian()
 */