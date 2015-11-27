-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

import qualified Data.MemoCombinators as Memo

collatz :: Integer -> Integer
collatz = Memo.arrayRange (1, 1000000) collatz'
  where
    collatz' 1 = 1
    collatz' x | even x    = 1 + collatz' (x `div` 2)
               | otherwise = 1 + collatz' (3 * x + 1)

m :: Integer
m = snd $ foldl1 max (zip cns [1..])
  where cns = map collatz [1..1000000]
