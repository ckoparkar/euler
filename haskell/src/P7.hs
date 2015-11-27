-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10 001st prime number?

-- Running time: 3.05 secs

module P7 where

import P3 (ld)

prime :: Integer -> Bool
prime n | n <= 1 = False
        | otherwise = ld n == n

primes :: [Integer]
primes = filter prime [1..]

m = primes !! 10000
