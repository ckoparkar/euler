-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

-- Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

-- d2,d3,d4=406 is divisible by 2
-- d3,d4,d5=063 is divisible by 3
-- d4,d5,d6=635 is divisible by 5
-- d5,d6,d7=357 is divisible by 7
-- d6,d7,d8=572 is divisible by 11
-- d7,d8,d9=728 is divisible by 13
-- d8,d9,d10=289 is divisible by 17
-- Find the sum of all 0 to 9 pandigital numbers with this property.

import P10 (primes)
import Data.Digits (unDigits)
import Data.List (permutations)

interesting :: [Integer] -> Bool
interesting xs =  all x as
  where as = [1..7]
        x a = (unDigits 10 [xs !! a, xs !! (a+1), xs !! (a+2)]) `mod` primes !! (a-1) == 0

m = sum $ map (unDigits 10) $ filter interesting (permutations [0..9])
