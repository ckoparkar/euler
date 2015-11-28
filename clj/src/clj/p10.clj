(ns clj.p10
  (:use [clj.core :only (primes)]))

"Find the sum of all the primes below two million."

(defn soln
  "Limit to sum primes"
  [n]
  (reduce + (take-while (partial > n) primes)))

;; 5261 msecs
(soln 2000000)
