-- A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

-- For example,

-- 44 → 32 → 13 → 10 → 1 → 1
-- 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

-- How many starting numbers below ten million will arrive at 89?

-- Running time: 91.25 secs

import Data.Digits(digits)
import qualified Data.MemoCombinators as Memo

square :: Integer -> Integer
square n = n * n

squareOfDigits :: Integer -> Integer
squareOfDigits n = foldl (\acc x -> acc + square x) 0 $ digits 10 n

chain89 :: Integer -> Bool
chain89 = Memo.integral chain89'
  where chain89' n | n == 89 = True
                   | n == 1 = False
                   | otherwise = chain89 $ squareOfDigits n

m = length $ filter chain89 [1..10000000]
