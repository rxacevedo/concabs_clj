(ns concabs.ch2
  (:gen-class))

;; Exercise 2.5
(defn mult [a b]
  "Multiplication in terms of addition"
  (let [or-zero? #(or (zero? %1) (zero? %2))]
    (cond (or-zero? a b) 0
          (neg? a) (- 0 (mult (- 0 a) b))
          (neg? b) (- 0 (mult a (- 0 b)))
          :else (+ a (mult a (dec b))))))

;; Around 2.3
(defn my-quot [a b]
  (cond (or (< a b ) (zero? a)) 0
        (neg? a) (- 0 (my-quot (- 0 a) b))
        (neg? b) (- 0 (my-quot a (- 0 b)))
        :else (inc (my-quot (- a b) b))))

;; Non tail-recursive
(defn sum-of-first [n]
  (if (zero? n) 0
      (+ n (sum-of-first (dec n)))))

;; Beginning of ch2 examples, made iterative
(defn factorial
  [n]
  "Compute factorial of n"
  (loop [a 1 b n]
    (if (<= b 0)
      a
      (recur (* a b) (dec b)))))

(defn num-digits [n]
  "Print how many digits in a number n"
  (loop [acc 1 n n]
    (if (< n 10) acc
        (recur (inc acc ) (quot n 10)))))

;; Non tail-recursive version
(defn presents-on-day-old [n]
  (if (= n 1) 1
      (+ n (presents-on-day-old (- n 1)))))

;; Exercise 2.17
(defn presents-on-day [n]
 (if (= n 1) n ;; Acc needs to be 0
     (loop [acc 0 n n]
       (if (zero? n) acc
         (recur (+ acc n) (dec n))))))
