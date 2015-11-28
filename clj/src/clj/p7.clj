(ns clj.p7
  (:use [clj.core :only (prime?)]))

(defn soln
  [n]
  (nth (filter prime? (range)) n))

;; 80 msecs
(soln 10001)
