from typing import List

class Solution:
    def minOperations(self, nums: List[int]) -> int:
        # should operate for each triple [height, less-then-self-idx-to-left, less-then-self-idx-to-right]
        # for each element, get [less-than-self] to the left and right.
        lefts = []
        stack = [(-1, -1)]
        for i, x in enumerate(nums):
            while True:
                idx, height = stack[-1]
                if height < x:
                    lefts.append(idx)
                    stack.append((i, x))
                    break
                else:
                    # height >= x
                    stack.pop()

        rights = []
        stack = [(len(nums), -1)]
        for i in reversed(range(len(nums))):
            x = nums[i]
            while True:
                idx, height = stack[-1]
                if height < x:
                    rights.append(idx)
                    stack.append((i, x))
                    break
                else:
                    # height >= x
                    stack.pop()
        rights.reverse()

        results = {(nums[i], l, r) for i, (l, r) in enumerate(zip(lefts, rights)) if nums[i] != 0}
        return len(results)


if __name__ == "__main__":
    print(Solution().minOperations([0, 2]))
    print(Solution().minOperations([3,1,2,1]))
    print(Solution().minOperations([1,2,1,2,1,2]))
    print(Solution().minOperations([4,1,6,4]))
