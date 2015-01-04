divides3or5 x = if x `mod` 3 == 0 || x `mod` 5 == 0
                then True
                else False


sum (filter divides3or5 (init [1..1000]))
