-- It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

import Data.Digits (digits)
import Data.List (sort,nub)

sameDigits :: [Integer] -> Bool
sameDigits xs = (== 1) $ length $ nub $ map d xs
  where d x = sort $ digits 10 x

candidates :: Integer -> [Integer]
candidates n = map (* n) [1..6]

m = head $ head $ filter sameDigits $ map candidates [1..]
