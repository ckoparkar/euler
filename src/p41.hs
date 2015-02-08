-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

-- What is the largest n-digit pandigital prime that exists?

import P32 (nPandigital)
import P10 (primes)
import Data.Digits (digits, unDigits)

candidates xs = filter (\x -> nPandigital ((fromIntegral . length) x) x) $ map (digits 10) xs

m = last $ map (unDigits 10) $ candidates $ take 1000000 primes
