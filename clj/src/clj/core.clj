(ns clj.core)

(defn divides?
  "Returns true if a is divisible by b"
  [a b]
  (zero? (mod a b)))

(defn fibonacci
  "Returns an infinite sequence of fibonacci numbers."
  ([] (fibonacci 1 2))
  ([a b]
   (cons a (lazy-seq (fibonacci b (+ b a))))))
