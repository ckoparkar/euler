-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

-- Running time: 5.16 secs

module P10 where
import qualified Data.Map as Map

sieve :: (Ord t, Num t) => [t] -> [t]
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

primes :: [Integer]
primes = sieve [2..]

m = sum $ takeWhile (< 2000000) primes
