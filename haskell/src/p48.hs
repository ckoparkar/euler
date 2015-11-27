-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

import Data.Digits

lastn :: Int -> [a] -> [a]
lastn n xs = reverse $ take n $ reverse xs

m = unDigits 10 $ lastn 10 $ digits 10 $ sum $ map (\x -> x ^ x) [1..1000]
