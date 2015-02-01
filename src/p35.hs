-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

-- How many circular primes are there below one million?

import P10 (primes)
import P7 (prime)
import Data.Digits

rotate :: Integer -> [Integer]
rotate n = rotate' (n : []) l d
  where l = subtract 1 $ length $ digits 10 n
        d = 10 ^ l

rotate' :: (Num a1, Integral a, Eq a1) => [a] -> a1 -> a -> [a]
rotate' ns len d
  | len == 0 = ns
  | otherwise = rotate' (y : ns) (len - 1) d
  where x = head ns
        y = x `mod` d * 10 + x `div` d

circular :: Integer -> Bool
circular n = and $ map prime cps
  where cps = rotate n

m = length $ filter circular $ takeWhile (< 1000000) primes
