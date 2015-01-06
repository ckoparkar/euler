module P7 where

import P3 (ld)

prime n | n == 1 = False
        | otherwise = ld n == n

primes = filter prime [1..]

m = primes !! 10000
