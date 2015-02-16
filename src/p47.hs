-- The first two consecutive numbers to have two distinct prime factors are:

-- 14 = 2 × 7
-- 15 = 3 × 5

-- The first three consecutive numbers to have three distinct prime factors are:

-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.

-- Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?

import P3 (factors)
import Data.List (nub)

uniqueFactors :: Integer -> [[Integer]]
uniqueFactors x = map nub [factors x, factors (x+1), factors (x+2), factors (x+3)]

m = head [x | x <- [1..], all ((== 4) . length) (uniqueFactors x)]
