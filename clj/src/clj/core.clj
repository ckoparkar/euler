(ns clj.core)

(defn divides?
  "Returns true if a is divisible by b"
  [a b]
  (zero? (mod a b)))
