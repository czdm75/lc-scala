package p155

class MinStack() {
  import scala.collection.mutable.Stack
  val minStack = Stack.empty[Int]
  val stack = Stack.empty[Int]

  def push(`val`: Int): Unit = {
    stack.push(`val`)
    minStack.push(if minStack.isEmpty then `val` else math.min(minStack.top, `val`))
  }

  def pop(): Unit = {
    stack.pop()
    minStack.pop()
  }

  def top(): Int = {
    stack.top
  }

  def getMin(): Int = {
    minStack.top
  }

}

/** Your MinStack object will be instantiated and called as such: val obj = new MinStack() obj.push(`val`) obj.pop() val
  * param_3 = obj.top() val param_4 = obj.getMin()
  */
