(ns concabs.ch2
  (:gen-class))

(defn factorial
  "Compute factorial of n"
  [n]
  (loop [b n a 1]
    (if (<= b 0)
      a
      (recur (- b 1) (* a b)))))

(defn num-digits [n]
  "Print how many digits in a number n"
  (loop [acc 1 n n]
    (if (< n 10) acc
        (recur (inc acc ) (quot n 10)))))

;; Non tail-recursive version
(defn presents-on-day-old [n]
  (if (= n 1) 1
      (+ n (presents-on-day-old (- n 1)))))

(defn presents-on-day [n]
 (if (= n 1) n ;; Acc needs to be 0
     (loop [acc 0 n n]
       (if (zero? n) acc
         (recur (+ acc n) (dec n))))))
