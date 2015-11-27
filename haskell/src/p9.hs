-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- Running time: 0.16 secs

triplets :: [Integer]
triplets  = [x * y * z |
             x <- [1..1000],
             y <- [1..x],
             let z = 1000 - x - y,
             x ^ 2 == y ^ 2 + z ^ 2]

m = head $ filter (> 0) triplets
