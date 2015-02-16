-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

module P3 where
import qualified Data.MemoCombinators as Memo

divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k ^ 2 > n   = n
        | otherwise   = ldf (k+1) n

factors :: Integer -> [Integer]
factors = Memo.integral factors'
  where factors' n | n == 1    = []
                   | otherwise = p : factors (div n p) where p = ld n

m = last $ factors 600851475143
