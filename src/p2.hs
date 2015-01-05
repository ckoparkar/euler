fibs = 0 : scanl (+) 1 fibs
evenAndLessFibs = filter even (takeWhile (< 4000000) fibs)
m = sum evenAndLessFibs
