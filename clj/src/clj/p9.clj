(ns clj.p9
  (:use [clj.core :only (square)]))

"A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc."

(defn soln
  "n is the desired sum of pythgorean triplet"
  [n]
  (first
   (for [x (range 1 n)
         y (range 1 x)
         :let [z (- n x y)]
         :when (= (square z) (+ (square x) (square y)))]
     (* x y z))))

;; 18 msecs
(soln 1000)
