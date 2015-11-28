(ns clj.p3
  (use [clj.core :only (prime-factors)]))

"The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?"

(defn soln
  [n]
  (last ((memoize prime-factors) n)))

;; 0.3 msecs
(soln 600851475143)
