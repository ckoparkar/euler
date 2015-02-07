-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

-- There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

-- If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

import Data.Ratio (denominator)

fraction n d = fromIntegral n / fromIntegral d

curious :: Integer -> Integer -> Bool
curious n d
  | n == d = False
  | fraction n d > 1 = False
  | n2 /= d1 = False
  | otherwise = fraction n1 d2 == fraction n d
  where (n1, n2) = quotRem n 10
        (d1, d2) = quotRem d 10

m = denominator $ product [fraction n d | n <- [10..99], d <- [10..99], curious n d]
