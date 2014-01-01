(ns concabs.ch4
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

;; Modular arithmetic
;; For each non-negative integer x, x mod m is the unique remainder r
;; of x when x is divided by r. Given m, it holds that:

;;;;;;;;;;;;;;;;
;; 0 <= r < m ;;
;;;;;;;;;;;;;;;;

;; It also holds that there is another integer q, such that:

;;;;;;;;;;;;;;;;
;; x = qm + r ;;
;;;;;;;;;;;;;;;;

;; If two integers differ by a multiple of m, they have the same
;; remainder mod m, or are in the same equivalence class. For example, take 35 and 13. 35 and 13 differ by
;; 22, which is a multiple of 11 (and 2).
;;
;; (mod (* 35 13) 11)
;; user> 4
;;
;; (mod 35 11)
;; user> 4
;;
;; (mod 13 11)
;; user> 4
;;
;; So we are not saying that they have a specific remainder, just that
;; they have the same remainder because the difference between the two
;; is a multiple of the modulus we used. Because of these rules, the
;; following can be shown to be true for all integers x and y:
;;
;; xy mod m = (x mod m)(y mod m) mod m
;; (x + y) mod m = ((x mod m) + (y mod m)) mod m

(def gold-num 5972304273877744135569338397692020533504)

(def signature 143676221783307728140118556730532825709962359695147398872633033728948225540940112091576952965868445265161373616153020167902900930324840824269164789456142215776895016041636987254848119449940440885630)

(def modulus 671629488048603400615365258174985654900765971941961654084193604750896012182890124354255484422321487634816640987992317596893099956961956383454333339584850276505584537663630293912940840460009374858969)

(def signing-exponent 447752992032402267076910172116657103267177314627974436056129069833930674788593416236170322948214322483305175278012793102392215895931470577163544613600143471679799876664686423606429437389098641670667)

(defn verify [sig m]
  "Signature verification"
  (mod (math/expt sig 3) m))

(defn mod+ [a b m]
  "Modular add"
  (mod (+ a b) m))

(defn mod* [a b m]
  "Modular multiply"
  (mod (* a b) m))

(defn mod- [a b m]
  "Modular subtract"
  (mod (+ a (- m b)) m))


;; Needs way too much memory to store intermediate results with large
;; e, produces StackOverflowError because this is not tail-recursive
(defn mod-expt-1 [b e m]
  "Modular exponentiation, linear recursive - O(e) memory/space, O(e) time"
  (if (zero? e) 1
      (mod* (mod-expt-1 b (dec e) m) b m)))

;; Uses O(1) memory, but still takes way too long for large e
;; because it is only decremented by one each time (this is not done
;; in the text).
(defn mod-expt-2 [b e m]
  "Modular exponentiation, linear iterative - O(1) memory/space, O(e) time"
  (loop [a 1 e e]
    (if (zero? e) a
        (recur (mod* a b m) (dec e)))))

;; This version is tree-recursive, but tree depth will only
;; be O(log e) because e is halved each time
(defn mod-expt-3 [b e m]
  "Modular exponentiation, tree-recursive - O(log e) memory/space, O(e) time (2e-1 multiplications)"
  (cond (zero? e) 1
        (even? e) (mod*
                   (mod-expt-3 b (/ e 2) m)
                   (mod-expt-3 b (/ e 2) m) m)
        :else (mod*
               (mod-expt-3 b (dec e) m) b m)))

;; This version is correct, it achieves a tree depth of log(e) and
;; uses memory proportional to log(e). This should be
;; improved/replaced with a logarithmic time iterative version
(defn mod-expt-4 [b e m]
  "Modular exponentiation, logarithmic recursion - O(log e) memory/space, O(log e) time"
  (cond (zero? e) 1
        (even? e) (let [half (mod-expt-4 b (/ e 2) m)]
                    (mod* half half m))
        :else (mod* (mod-expt-4 b (dec e) m) b m)))

(defn mod-expt [b e m]
  "Modular exponentiation, logarithmic iteration - O(1) memory/space, O(log e) time"
  (loop [a 1 e e b b]
    (cond (zero? e) a
          (even? e) (let [iter (/ e 2)
                          p (mod* b b m)]
                      (recur (mod* a p m) (dec iter) p))
          :else (recur (mod* a b m) (dec e) b))))

;; Excercise 4.12
;; There are three cases to consider for this exercise:
;; (i). When m^2 > n, there are 0 ways to factor n using numbers no
;; smaller than m.
;; (ii). When n is not divisible by m, then the number of ways to factor
;; n using numbers no smaller than m is the same as the number of ways
;; to factor n using numbers no smaller than m + 1 (because you are
;; using the same set of numbers to factor except you are leaving out
;; m, which was not a divisor of n anyways).
;; (iii). When m^2 <= n, there is at least one way to factor n using
;; numbers no smaller than m (m and n/m).
(defn ways-to-factor-using-no-smaller-than [n m]
  "Returns the number of distinct ways to factor n using numbers no smaller than m."
  (loop [acc 0 a n b m]
       (cond (> (math/expt b 2) a) acc
             (zero? (mod a b))
                    (let [sub-factors (ways-to-factor-using-no-smaller-than (/ a b) b)]
                              (recur (+ (inc acc) sub-factors) a (inc b)))
          :else (recur acc a (inc b)))))

(defn ways-to-factor [n]
  "Returns the number of distinct ways to factor n using numbers no smaller than 2."
  (ways-to-factor-using-no-smaller-than n 2))

;; Exercise 4.15
(defn max-weighings [& args]
  "Returns the number of weighings to determine which coin is fake (exercise 4.15)."
  (let [sum #(reduce + %)]
    (loop [acc 0 coll args]
      (let [sz (count coll)
            r (mod sz 2)
            psize (/ (- sz r) 2)]
        (cond (= sz 1) acc
              (zero? r) (let [[p1 p2] (partition psize coll)]
                          (prn p1 p2)
                          (if (< (sum p1) (sum p2)) (recur (inc acc) p1)
                              (recur (inc acc) p2)))
              :else     (let [[p1 p2] (partition psize (take (- sz r) coll))
                              p3 (seq (drop (- sz r) coll))]
                          (prn p1 p2 p3)
                          (cond (< (sum p1) (sum p2)) (recur (inc acc) p1)
                                (> (sum p1) (sum p2)) (recur (inc acc) p2)
                                :else (recur (inc acc) p3))))))))

(defn max-weighings-thirds [& args]
  "Returns the number of weighings required to determine which coin is fake.
   This approach uses three piles instead of two."
  (let [sum #(reduce + %)]
    (loop [acc 0 coll args]
      (let [sz (count coll)
            r (mod sz 3)
            psize (/ (- sz r) 3)]
        (cond (= sz 1) acc
              (= sz 2) (let [[p1 p2] (partition 1 coll)]
                         (prn p1 p2)
                         (if (< (sum p1) (sum p2)) (recur (inc acc) p1)
                             (recur (inc acc) p2)))
              (zero? r) (let [[p1 p2 p3] (partition psize coll)]
                          (prn p1 p2 p3)
                          (cond (< (sum p1) (sum p2)) (recur (inc acc) p1)
                                (> (sum p1) (sum p2)) (recur (inc acc) p2)
                                :else (recur (inc acc) p3)))
              :else     (let [[p1 p2] (partition psize (take (* 2 psize) coll))
                              p3 (drop (* 2 psize) coll)]
                          (prn p1 p2 p3)
                          (cond (< (sum p1) (sum p2)) (recur (inc acc) p1)
                                (> (sum p1) (sum p2)) (recur (inc acc) p2)
                                :else (recur (inc acc) p3))))))))

;; Exercise 4.17, nCr/Pascal's triangle (r,c)
(defn choose [n k]
  "Returns the number of ways to choose k elements from a set of n elements."
  (cond (or (zero? k) (= n k)) 1
        (zero? n) 0
        :else (+ (choose (dec n) (dec k)) (choose (dec n) k))))

;; Exercise 4.18
(defn sum-ints [a b]
  "Returns the sum of the integers from a to b, or
   (reduce + (range a (inc b)))"
  (let [range (- b a)]
    (cond (zero? (- b a)) a
          (zero? (mod range 2)) (let [a1 a
                                      b1 (+ a (/ range 2))
                                      a2 (inc b1)
                                      b2 b]
                                  (+ (sum-ints a1 b1) (sum-ints a2 b2)))
          :else                 (let [a1 a
                                      b1 (+ a (/ (dec range) 2))
                                      a2 (inc b1)
                                      b2 b]
                                  (+ (sum-ints a1 b1) (sum-ints a2 b2))))))

