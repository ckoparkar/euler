(ns clj.p14)

"The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million."

(def collatz
  "Returns size of collatz sequence for n"
  (memoize (fn [n]
             (cond
               (= n 1) 1
               (even? n) (inc (collatz (quot n 2)))
               :else (inc (collatz (inc (* 3 n))))
               ))))

(defn soln
  "n is limit"
  [n]
  ((comp first first)
        (sort-by second >
                 (reduce #(conj % [%2 (collatz %2)]) [] (range 1 n)))))

;; 7058 msecs
(soln 1000000)
