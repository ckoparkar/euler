(ns clj.p7
  (:use [clj.core :only (primes)]))

"By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?"

(defn soln
  [n]
  (nth primes n))

;; 80 msecs
(soln 10001)
