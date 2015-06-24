-- The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

-- How many n-digit positive integers exist which are also an nth power?
-- Running time: 0.06 secs

m = length [1 | a <- [1..99], b <- [1..99], (length $ show (a^b)) == b]
