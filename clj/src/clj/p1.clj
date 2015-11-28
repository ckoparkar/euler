(ns clj.p1
  (:use [clj.core :only (divides?)]))

"If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000."

(defn soln
  [n]
  (reduce
   #(if (or (divides? %2 3) (divides? %2 5)) (+ %1 %2) %1)
   0
   (range n)
   ))

;; 0.4 msecs
(soln 1000)
