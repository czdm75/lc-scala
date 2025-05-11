class Solution:
    def maxFreqSum(self, s: str) -> int:
        vowels = {'': 0}
        consonats = {'': 0}
        for c in s:
            if c in "aeiou":
                vowels[c] = vowels.get(c, 0) + 1
            else:
                consonats[c] = consonats.get(c, 0) + 1
        return max(x for x in vowels.values()) + max(x for x in consonats.values())
