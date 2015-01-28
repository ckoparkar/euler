-- You are given the following information, but you may prefer to do some research for yourself.

-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

-- Method:
-- Sunday is 0, Monday is 1 ...
-- If Jan 1 1900 is Monday, Feb 1 1900 would be (1+31) % 7 = 4, i.e Thursday

isLeapYear :: Int -> Bool
isLeapYear n = n `mod` 4 == 0 &&
               (n `mod` 400 == 0 ||
               n `mod` 100 /= 0)

days :: [Integer]
days = [31,28,31,30,31,30,31,31,30,31,30,31]

leapDays :: [Integer]
leapDays = map (\x -> if x == 28 then 29 else x) days

flatten :: [[Integer]] -> [Integer]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

daysOfAllYears :: [Int] -> [Integer]
daysOfAllYears xs = flatten $
                    map (\x -> if isLeapYear x then leapDays else days) xs

whichDay :: Integer -> [Integer] -> [Integer]
whichDay _ [] = []
whichDay d (x:xs) = d : whichDay ((d+x) `mod` 7) xs

sundays :: [Integer] -> Int
sundays xs = length $ filter (== 0) xs

m :: Int
m = sundays $ whichDay 2 $ daysOfAllYears [1901..2000]
