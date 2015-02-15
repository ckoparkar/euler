-- Take the number 192 and multiply it by each of 1, 2, and 3:

-- 192 × 1 = 192
-- 192 × 2 = 384
-- 192 × 3 = 576
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

import Data.Digits (digits, unDigits)
import P32 (nPandigital)
import Data.Function (on)
import Data.List (maximumBy)

multiples :: Integer -> [Integer]
multiples x = concat $ takeWhile ((== 9) . length) $
              dropWhile ((< 9) . length) $
              scanl (\acc y -> acc ++ digits 10 (x*y)) (digits 10 x) [2..]

m = unDigits 10 $
    maximumBy (compare `on` (unDigits 10)) $
    filter (nPandigital 9) $
    map multiples [1..10000]
