-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import P7 (prime)
import P10 (primes)
import Data.Digits

truncatableR :: Integer -> Bool
truncatableR n
  | n <= 0 = True
  | prime n = truncatableR (n `div` 10)
  | otherwise = False

log10 :: Integer -> Integer
log10 n = floor $ logI n / logI 10
  where logI = log . fromIntegral

truncatableL :: Integer -> Bool
truncatableL n
  | n <= 0 = True
  | prime n = truncatableL (n `mod` y)
  | otherwise = False
  where x = log10 n
        y = 10 ^ x


ps = drop 4 primes
m = sum $ filter (\x -> truncatableL x && truncatableR x) (take 100000 ps)
