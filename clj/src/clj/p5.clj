(ns clj.p5
  (:use [clj.core :only (lcm)]))

"2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"

(defn soln
  [n]
  (reduce lcm (range 1 n)))

;; 0.2 msecs
(soln 20)
