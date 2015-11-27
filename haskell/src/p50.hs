-- The prime 41, can be written as the sum of six consecutive primes:

-- 41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.

-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

import P7 (prime)
import P10 (primes)
import Data.List (maximumBy)
import Data.Function (on)

candidates p = dropWhile (not . prime) $ reverse sums
  where sums = takeWhile (< 1000000) $ scanl1 (+) $ dropWhile (< p) primes

m = head $ maximumBy (compare `on` length) $ map candidates $ take 10 primes
