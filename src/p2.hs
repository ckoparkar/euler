fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

fibs = takeWhile (< 4000000) [fib x | x <- [1..]]

evenfibs = filter even fibs

-- sum evenfibs
