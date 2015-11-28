(ns clj.p6
  (:use [clj.core :only (square)]))

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
