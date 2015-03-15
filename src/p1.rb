# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

# Find the sum of all the multiples of 3 or 5 below 1000.

# Running time: 0.02 secs

p (1..999).lazy.select {|x| x.remainder(3) == 0 || x.remainder(5) == 0}.reduce(&:+)
