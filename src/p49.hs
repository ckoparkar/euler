-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

-- What 12-digit number do you form by concatenating the three terms in this sequence?

import P7 (prime)
import P10 (primes)
import P52 (sameDigits)

fourPrimes = takeWhile (< 9999) $ dropWhile (< 1000) primes

m = concatMap show $ last $
    filter (\x -> sameDigits x) $
    filter (\x -> all prime x) $
    [[p, p+3330, p+6660] | p <- fourPrimes]
