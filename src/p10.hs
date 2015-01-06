-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

import qualified Data.Map as Map

sieve [] = []
sieve xs = sieve' xs Map.empty
  where
    sieve' [] table  = []
    sieve' (x:xs) table =
      case Map.lookup x table of
       Nothing    -> x : sieve' xs (Map.insert (x*x) [x] table)
       Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
         where
           reinsert table prime = Map.insertWith (++) (x + prime) [prime] table

primes = sieve [2..]

m = sum $ takeWhile (< 2000000) primes
