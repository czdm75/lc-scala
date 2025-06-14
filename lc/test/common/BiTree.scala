package common

import utest._
import common._

object BiTreeTests extends TestSuite {
  def tests = Tests {
    test("some") {
      val tree = new ArrayBasedBiTree(Array(1, 2, 3, 4, 5), 0) with RecursiveTraverse
      println(tree)
      assert(tree.breadthFirstTraverse == List(1, 2, 3, 4, 5))
    }
  }
}
