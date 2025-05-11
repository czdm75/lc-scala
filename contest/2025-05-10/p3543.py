from typing import List

class Solution:
    def maxWeight(self, n: int, edges: List[List[int]], k: int, t: int) -> int:
        if k == 0:
            return 0

        mm = dict()
        for u, v, w in edges:
            if u not in mm:
                mm[u] = dict()
            mm[u][v] = w

        # when path_length = 1
        path_dict = dict()
        for fr, rest in mm.items():
            path_dict[fr] = dict()
            for to, w in rest.items():
                path_dict[fr][to] = {w}


        # iterate over path_length
        for x in range(1, k):
            new_d = dict()
            for start, rest in path_dict.items():
                new_d[start] = dict()
                for end, weights in rest.items():
                    if end not in mm:
                        continue
                    for new_end, add_weight in mm[end].items():
                        for old_weight in weights:
                            if new_end not in new_d[start]:
                                new_d[start][new_end] = set()
                            new_d[start][new_end].add(old_weight + add_weight)
            path_dict = new_d
        results = {w for _, rest in path_dict.items() for _, ws in rest.items() for w in ws if w < t}
        if not results:
            return -1
        else:
            return(max(results))

if __name__ == "__main__":
    print(Solution().maxWeight(n = 3, edges = [[0,1,1],[1,2,2]], k = 2, t = 4))
    print(Solution().maxWeight(n = 3, edges = [[0,1,2],[0,2,3]], k = 1, t = 3))
    print(Solution().maxWeight(n = 3, edges = [[0,1,6],[1,2,8]], k = 1, t = 6))
    print(Solution().maxWeight(1, [], 0, 287))
