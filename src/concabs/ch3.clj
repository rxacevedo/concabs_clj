(ns concabs.ch3
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

(defn factorial-old [n] ;; Non tail-recursive version
  (if (zero? n) 1
      (* n (factorial-old (dec n)))))

;; 3.1 Concabs
(defn factorial [n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* n acc) (dec n)))))

;; This may not be right since it starts at index 0
(defn fermat-number [n]
  (loop [acc 2 n n]
    (if (zero? n) (inc acc)
        (recur (math/expt acc 2) (dec n)))))

;; 3.3 Concabs
(defn divides? [a b]
  "Returns true if b divides evenly into a"
  (zero? (mod a b)))


;; 3.3 Concabs
(defn perfect? [n]
  "Determine whether n is a perfect number"
  (let [acc 0
        start 1
        sum-of-divisors (fn [acc start]
                          (if (> acc n) acc
                              (recur (if (divides? n start) (+ acc start)
                                         acc)
                                     (inc start))))]
    (= (sum-of-divisors acc start) (* 2 n))))

;; Exercise 3.6 Concabs
(defn first-perfect-after [n]
  "Returns the first perfect number after n"
  (let [a (inc n)]
    (loop [a a]
      (if (perfect? a) a
          (recur (inc a))))))

;; Ex 3.7 Concabs
(defn improve [approx]
  (+ 1 (/ 1 approx)))

;; How the fuck can I represent 1/1?
(defn approximate-golden-ratio [tolerance]
  (let [f (fn good-enough? [b]
            (< (/ 1 (int (math/expt (denominator b) 2)) ) tolerance))]
    (loop [start 1/1]
      (if (f start) start
          (recur (improve start))))))
;; 3.5 Concabs
(defn survives? [pos size]
  ;; (println (str "Size: " size ", position: " pos))
  (if (= pos 3) false
      (let [renumber (fn [pos size]
                       (cond (> pos 3) (- pos 3)
                             (= pos 1) (- size 2)
                             (= pos 2) (- size 1)
                             :else -1))]
        (if (and (< pos 3) (<= size 3)) true
            (recur (renumber pos size) (dec size))))))

(defn find-survivors [size]
  "Find survivors in a group of size size per the Josephus problem"
  (let [survivors nil]
    (loop [pos size survivors nil]
      (if (>= pos 1)
        (recur (dec pos) (if (survives? pos size)
                           (conj survivors pos)
                           survivors))
        (println (str "Survivors: " (clojure.string/join ", " survivors)))))))

;; Exercise 3.17 Concabs
(defn falling-factorial [n k]
  "Calculate the falling factorial of n falling k"
  (loop [acc 1 n n k k]
    (if (zero? k) acc
        (recur (* acc n) (dec n) (dec k)))))

