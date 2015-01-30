-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

a :: [(Integer, Integer)]
a = [(0,0), (1,3), (2,3), (3,5), (4,4), (5,4), (6,3), (7,5), (8,5), (9,4)]

b :: [(Integer, Integer)]
b = [(10, 3), (11,6), (12,6), (13,8), (14,8), (15,7), (16,7), (17,9), (18,8), (19,8)]

c :: [(Integer, Integer)]
c = [(20,6), (30,6), (40,5), (50,5) , (60,5), (70,7), (80,6), (90,6)]

get :: (Num b, Eq a) => a -> [(a, b)] -> b
get k table =
  case lookup k table of
   Just v -> v
   Nothing -> 0

letters :: Integer -> Integer
letters n
  | n < 10 = get n a
  | n < 20 = get n b
  | n < 100 = get (n `div` 10 * 10) c + get (n `mod` 10) a
  | n == 1000 = 11
  | otherwise = x + get (n `div` 100) a + letters (n `mod` 100)
  where x = if n `mod` 100 == 0 then 7 else 10

m = sum $ map letters [1..1000]
