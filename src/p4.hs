-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

-- Running time: 0.37 secs

ps :: [Integer]
ps = [x*y | x <- [100..999], y <- [x..999]]

isPalindrome :: Show a => a -> Bool
isPalindrome x = y == reverse y
  where y = show x

m = maximum $ filter isPalindrome ps
