(ns concabs.ch5
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math])
  (:use concabs.ch4))

;; Notice the pattern shown in the below two functions, they can be
;; refactored to us a higher-order function of the same form.

(defn pow [b e]
  (if (zero? e) 1
      (* b (pow b (dec e)))))

(defn sum-copies [n c]
  (if (zero? c) 0
      (+ n (sum-copies n (dec c)))))

;; Note: These are written to be used with commutative functions.

;; Same as together-copies-of, p. 111
(defn combine-1 [f a b d]
  "Combines b copies of a using f with d as the base case value.
   O(e) memory/space, O(e) time."
  (if (zero? b) d
      (f a (combine-1 f a (dec b) d))))

;; Exercise 5.1
(defn combine-2 [f a b d]
  "Combines b copies of a using f with d as the initial accumulator value.
   O(1) memory/space, O(e) time."
  (loop [acc d b b]
    (if (zero? b) acc
        (recur (f acc a) (dec b)))))

;; Exercise 5.2
(defn combine-3 [f a b d]
  "Combines b copies of a using f with d as the base case value.
   O(log e) memory/space, O(log e) time."
  (cond (zero? b) d
        (even? b) (let [half (combine-3 f a (/ b 2) d)]
                    (f half half))
        :else (f a (combine-3 f a (dec b) d))))

;; Extra
(defn combine-4 [f a b d]
  "Combines b copies of a using f with d as the initial accumulator value.
   O(1) memory/space, O(log e) time."
  (loop [acc d a a b b]
    (cond (zero? b) acc
          (even? b) (let [iters (/ b 2)
                          pair (f a a)]
                      (recur (f acc pair) pair (dec iters)))
          :else (recur (f acc a) a (dec b)))))

;; These can all now be written to use the new/much more efficient
;; version of combine.
(defn pow-2 [b e]
  (combine-4 * b e 1))

(defn sum-copies-2 [n c]
  (combine-4 + n c 0))

;; Not sure how many times I've revised this one.
(defn mod-expt-? [b e m]
  (combine-4 #(mod* %1 %2 modulus) b e 1))

;; Exercise 5.7
(defn make-exponentiater [e]
  #(math/expt % e))

(def square (make-exponentiater 2))

(def cube (make-exponentiater 3))

;; Exercise 5.7 continued
(defn make-repeated-version-of [f]
  (let [repeated (fn [b n] (loop [b b n n]
                            (if (zero? n) b
                                (recur (f b) (dec n)))))]
    repeated))

(def repeatedly-square (make-repeated-version-of square))

(defn fermat [n]
  "Returns the nth Fermat number, starting at zero."
  (inc (repeatedly-square 2 n)))

;; Exercise 5.8
(defn combine-down [f hi]
  (let [d (cond (= f *) 1
                (= f +) 0
                :else (throw (Exception. "No default available for this fn.")))]
    (if (zero? hi) d
        (f hi (combine-down f (dec hi))))))

;; Exercise 5.9
(defn combine-down-2 [f hi]
  "Returns a function that applies f from hi down to zero. The returned function
   accepts an fn transform that is applied to each integer passed through f.
   This is the same as (reduce f (map transform '(my coll of ints)))"
  (let [d (cond (= f *) 1
                (= f +) 0
                :else (throw (Exception. "No default available for this fn.")))
        inner (fn [transform] (if (zero? hi) d
                                                   (f (transform hi)
                                                      ((combine-down-2 f (dec hi)) transform))))]
    inner))

(defn combine-down-3 [f m]
  "Takes a function f and a high bound m where f is applied from m down to zero.
   Returns a new function that takes t-fn (transform) and r-fn (reduce) that
   are used to combine (t-fn m) with (combine-down-3 f (r-fn m))."
  (let [d (cond (= f +) 0
                (= f *) 1
                :else (throw (Exception. "No default available for this fn.")))
        inner (fn [t-fn r-fn]
                (if (zero? m) d
                    (f (t-fn m) ((combine-down-3 f (r-fn m)) t-fn r-fn))))]
    inner))

(defn factorial [n]
  "Returns the factorial of n. Uses the combine-down function."
  ((combine-down-3 * n) #(Long. %) dec))

(defn sum-of-squares [hi]
  "Returns the sum of squares from hi down to zero."
  ((combine-down-3 + hi) square dec))

(defn sum-of-cubes [hi]
  "Returns the sum of cubes from hi down to zero."
  ((combine-down-3 + hi) cube dec))

(defn sum-of-digits [n]
  "Returns the sum of the digits in a number"
  (loop [acc 0 m n]
    (if (zero? m) acc
        (let [digit (mod m 10)
              divisible (- m digit)]
          (recur (+ acc digit) (/ divisible 10))))))

;; TODO: This does not work correctly for numbers with leading zeros.
;; (mod 0262010771 10) returns 3 when it should return 1, which is causing
;; the total to be calculated incorrectly. This is because in Java, a leading
;; zero signifies an octal literal. Ex: (Long. 0262010771) returns 46666233.
(defn make-verifier [f m]
  "Returns a verifier for a type of ID number. Takes f and m and returns a fn
   that calculates the sum of digits with f applied to each and checks that the
   result is divisible by m. If it is divisible by m, it is valid. NOTE: This
   does not work with numbers that have leading zeros currently."
  (let [inner (fn [num]
                (loop [acc 0
                       n num
                       c 1]
                  (if (zero? n) acc
                      (let [digit (mod n 10)
                            happy-val (- n digit)]
                        (recur (+ acc (f c digit)) (/ happy-val 10) (inc c))))))]
    #(if (zero? (mod (inner %) m)) true
      false)))

;; Ex: (check-isbn 8090273416)
(def check-isbn
  "Returns true if the argument is a valid ISBN-10 number."
  (make-verifier * 11))

;; Ex: (check-upc 663595981057)
(def check-upc
  "Returns true if the argument is a valid UPC code."
  (make-verifier #(if (even? %1) (* 3 %2) %2) 10))

;; Ex: (check-credit-card 4111111111111111)
(def check-credit-card
  "Returns true if the argument is a valid credit card number."
  (make-verifier #(if (odd? %1) %2
                      (if (< %2 5) (* 2 %2)
                          (inc (* 2 %2)))) 10))

;; Ex: (check-pmo-serial 48077462766)
(def check-pmo-serial
  "Returns true if the argument is a valid postal money order serial number."
  (make-verifier #(if (= 1 %1) (- 0 %2) %2) 9))
