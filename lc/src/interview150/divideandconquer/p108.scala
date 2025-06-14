package p108

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def sortedArrayToBST(nums: Array[Int]): TreeNode = {
    def sliceToBST(from: Int, to: Int): TreeNode = {
      if (from == to) {
        null
      } else if (from + 1 == to) {
        TreeNode(nums(from))
      } else {
        val mid = (from + to) / 2
        TreeNode(nums(mid), sliceToBST(from, mid), sliceToBST(mid + 1, to))
      }
    }
    sliceToBST(0, nums.size)
  }
}
