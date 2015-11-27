-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.

-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.MemoCombinators (integral)
import Data.Digits

factorial :: Integer -> Integer
factorial = integral fact'
  where
    fact' n
      | n == 0 = 1
      | otherwise = n * fact' (n-1)

factDigits :: Integer -> [Integer]
factDigits n = map factorial (digits 10 n)

m = sum $ filter (\x -> x == (sum . factDigits) x) [3..1000000]
