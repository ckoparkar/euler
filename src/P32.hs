-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

-- Approach:
-- The first thought was to get Set.difference on [1..9] and digits, but it doesnt consider the numbers containing 0's.

module P32 where
import Data.Digits (digits)
import Data.List

nPandigital :: Integer -> [Integer] -> Bool
nPandigital n xs = (== [1..n]) . sort $ xs

products :: Integral t => t -> t -> [([t], t)]
products xs ys = [(concat [d x,d y,d (x*y)], x*y) | x <- [1..xs], y <- [1..ys]]
  where d as = digits 10 as

m = sum $ nub $ [p | (digits, p) <- (products 50 2000), nPandigital 9 digits]
