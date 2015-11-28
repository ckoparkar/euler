(ns clj.p9
  (:use [clj.core :only (square)]))

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
