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

(defn least-prime-factor
  "The least natural number greater than 1 that divides n"
  ([n] (least-prime-factor 2 n))
  ([k n]
   (cond
     (divides? n k) k
     (> (Math/pow k 2) n) n ;; means n is a prime number
     :else (least-prime-factor (inc k) n))))

(defn prime-factors
  "Returns list of prime factors of n"
  [n]
  (let [p (least-prime-factor n)]
    (if (= n 1) nil
        (cons p (lazy-seq (prime-factors (/ n p)))))))

(defn palindrome?
  "Returns if a n is palindrome"
  [n]
  (= (reverse (reverse n)) (reverse n)))

(defn digits
  "Returns list of digits of a int"
  [n]
  (map #(Character/getNumericValue %) (str n)))

(defn abs
  "(abs n) is the absolute value of n"
  [n]
  (cond
    (neg? n) (- n)
    :else n))

(defn gcd
  "(gcd a b) returns the greatest common divisor of a and b"
  [a b]
  (loop [a (abs a) b (abs b)]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn square
  "Return square of a"
  [n]
  (* n n))

(defn prime?
  [n]
  "Returns true if n is prime"
  (= n (least-prime-factor n)))
