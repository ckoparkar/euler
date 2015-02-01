-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

-- How many circular primes are there below one million?

import P10 (primes)
import P7 (prime)
import Data.Digits
import qualified Data.Map as Map

rotate :: Integer -> [Integer]
rotate n = rotate' (n : []) l d
  where l = subtract 1 $ length $ digits 10 n
        d = 10 ^ l

rotate' :: (Num a1, Integral a, Eq a1) => [a] -> a1 -> a -> [a]
rotate' ns len d
  | len == 0 = ns
  | otherwise = rotate' (y : ns) (len - 1) d
  where x = head ns
        y = x `mod` d * 10 + x `div` d

markAllRotations :: Integer -> a -> Map.Map Integer a -> Map.Map Integer a
markAllRotations n b mp =
  let rots = rotate n
      mar [] m = m
      mar (x:xs) m = mar xs (Map.insert x b m)
  in mar rots mp

circular' x = and $ map prime (rotate x)

circular :: [Integer] -> Map.Map Integer Bool -> Integer -> Integer
circular [] table sum = sum
circular (n:ns) table sum =
  case Map.lookup n table of
   Just v ->
     if v then
       circular ns table (sum + 1)
     else
       circular ns table sum
   Nothing ->
     if b then
       circular ns (markAllRotations n b table) (sum + 1)
     else
       circular ns (markAllRotations n b table) sum
     where b = circular' n
