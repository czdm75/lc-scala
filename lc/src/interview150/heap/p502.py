from typing import List

class Solution:
    def findMaximizedCapital(self, k: int, w: int, profits: List[int], capital: List[int]) -> int:
        from heapq import heappop, heappush
        projects = sorted(zip(profits, capital), key=lambda x: x[1])
        candidates = []
        i = 0
        for _ in range(k):
            while i < len(projects) and projects[i][1] <= w:
                heappush(candidates, -projects[i][0])
                i += 1
            if len(candidates) > 0:
                w -= heappop(candidates)
        return w

if __name__ == "__main__":
    print(Solution().findMaximizedCapital(k=2,w=0,profits=[1,2,3], capital=[0,1,1]))