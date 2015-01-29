-- A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

-- 1/2	= 	0.5
-- 1/3	= 	0.(3)
-- 1/4	= 	0.25
-- 1/5	= 	0.2
-- 1/6	= 	0.1(6)
-- 1/7	= 	0.(142857)
-- 1/8	= 	0.125
-- 1/9	= 	0.(1)
-- 1/10	= 	0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

import P10 (primes)
import Data.List

multiplicativeOrder :: Integral a => a -> a -> a
multiplicativeOrder i n
  | (10 ^ i) `mod` n == 1 = i
  | i > n = 0
  | otherwise =  multiplicativeOrder (i+1) n

primesL1000 :: [Integer]
primesL1000 = takeWhile (< 1000) primes

decimalPeriod :: [Integer]
decimalPeriod = map (multiplicativeOrder 1) primesL1000

m = snd $ foldl1 max $ zip decimalPeriod primesL1000
