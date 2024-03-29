(ns clj.p4
  (:use [clj.core :only (palindrome? digits)]))

"A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers."


(defn- products
  "Returns all products of 3 digit numbers"
  []
  (for [x (range 100 999)
        y (range 100 999)]
    (* x y)))

(defn soln
  []
  (apply max (filter #(palindrome? (digits %)) (products))))

;; TODO(cskksc): optimize
;; 19348 msecs
(soln)
