class Solution:
    def twoSum(self, nums: List[int], target: int) -> List[int]:
        #Optimized Soln: 1.) find the difference between the target - number
        # we're checking that gives us a number for us to check
        # 2.) put the key of our hash map as the difference and the 
        # 3.) check if this is number is in 
        # 4.) If no matches exist THEN add it to the hashmap
        prevMap = {}
        for index, num in enumerate(nums):
            diff = target - num
            print(prevMap)
            if diff in prevMap:
                return [prevMap[diff], index]
            # So only add the num and the index AFTER 
            prevMap[num] = index
            # print(diff)
solution = Solution()
print(solution.twoSum([2, 7, 11, 15], 9))
        # Output: [0,1]
        # Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
        # Example 2:
        # Input: nums = [3,2,4], target = 6
        # Output: [1,2]
        # Example 3:
        # Input: nums = [3,3], target = 6
        # Output: [0,1]

