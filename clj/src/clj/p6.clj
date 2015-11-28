(ns clj.p6
  (:use [clj.core :only (square)]))

"The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."

(defn sum-of-square
  "Return 1^2 + 2^2 ... + n^2"
  [n]
  (reduce #(+ %1 (square %2)) (range (inc n))))

(defn square-of-sum
  "Return (1+2+...+n)^2"
  [n]
  (square (reduce + (range (inc n)))))

(defn soln
  [n]
  (- (square-of-sum n) (sum-of-square n)))

;; 0.3 msecs
(soln 100)
