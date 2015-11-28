(ns clj.p7
  (:use [clj.core :only (primes)]))

(defn soln
  [n]
  (nth primes n))

;; 80 msecs
(soln 10001)
