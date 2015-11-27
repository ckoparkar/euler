-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include leading zeros.)

module P36 where
import Data.Digits

palindrome :: Integer -> Integer -> Bool
palindrome n b = x == reverse x
  where x = digits b n

m = sum $ filter (\x -> palindrome x 10 && palindrome x 2) [1..1000000]
