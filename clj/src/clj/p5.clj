(ns clj.p5
  (use [clj.core :only (lcm)]))

(defn soln
  [n]
  (reduce lcm (range 1 n)))

;; 0.2 msecs
(soln 20)
