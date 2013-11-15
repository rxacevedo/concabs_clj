(ns concabs.ch4
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math])
  (:import [javax.swing JPanel JFrame]
           [java.awt Dimension]))

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
  (loop [a 1
         e e]
    (if (zero? e) a
        (recur (mod* a b m) (dec e)))))

;; This version is tree-recursive, but tree depth will only
;; be O(log e) because e is halved each time
(defn mod-expt-3 [b e m]
  "Modular exponentiation, tree-recursive - O(log e) memory/space, O(e) time (2e-1 multiplications)"
  (cond (zero? e) 1
        (even? e) (mod*
                   (mod-expt-3 b (/ e 2) m)
                   (mod-expt-3 b (/ e 2) m)
                   m)
        :else (mod*
               (mod-expt-3 b (dec e) m)
               b m)))

;; This version is correct, it achieves a tree depth of log(e) and
;; uses memory proportional to log(e). This should be
;; improved/replaced with a logarithmic time iterative version
(defn mod-expt-4 [b e m]
  "Modular exponentiation, logarithmic recursion - O(log e) memory/space, O(log e) time"
  (cond (zero? e) 1
        (even? e) (let [half (mod-expt-4 b (/ e 2) m)] ;; Beautiful.
                    (mod* half half m))
        :else (mod* (mod-expt-4 b (dec e) m) b m)))

;; Exercise 4.4, the final, logarithmic time iterative version. e is decremented by
;; 1 initially to prevent the extra multiplation step
;; (b3 * (b1 * b1) * b0) <-- We do not want that 0 because when b2 is
;; reduced to (b1 * b1) we have no multiplications left to do, so we
;; settle for ((b1 * b1) * b0) instead
;; (defn mod-expt [b e m]
;;   "Modular exponentiation, logarithmic iteration - O(1) memory/space, O(log e) time"
;;   (loop [a 1
;;          e e]
;;     (cond (zero? e) a
;;           (even? e) (recur (mod* a (mod* b b m) m) (/ e 2))
;;           :else (recur (mod* a b m) (dec e)))))

;; (defn mod-expt-final-old [b e m]
;;   (if (even? e)
;;     (let [pair (mod* b b m)]
;;       (loop [iter (/ e 2)
;;              a 1]
;;         (if (zero? iter) a
;;             (recur (dec iter) (mod* a pair)))))
;;       (mod* b (mod-expt-final b (dec e) m) m)))

;; TODO: This is slower than mod-expt-4, why? Maybe consider what
;; happens when (mod e 2) is zero.
(defn mod-expt [b e m]
  "Modular exponentiation, logarithmic iteration - O(1) memory/space, O(log e) time"
  (loop [a 1
         e e
         b b]
    (cond (zero? e) a
          (even? e)
          (let [iter (/ e 2)
                p (mod* b b m)]
            (recur (mod* a p m) (dec iter) p))
          :else (recur (mod* a b m) (dec e) b))))

;; Fractal stuff goes here

;; (defn draw-rec [level]
;;   (let [panel (proxy [JPanel] []
;;                 (paintComponent [g]
;;                   (loop [x1 0 y1 0 x2 100 y2 100 lvl level]
;;                     (if (zero? lvl) 
;;                       (.drawLine g x1 y1 x2 y2)
;;                       (recur (inc x1) (inc y1) (inc x2) (inc y2) (dec level))))))]
;;     (doto panel
;;       (.setPreferredSize (Dimension. 400 400)))))
 
;; (defn make-frame [panel]
;;   (doto (new JFrame)
;;     (.add panel)
;;     .pack
;;     .show))

;; Excercise 4.12
;; There are three cases to consider for this exercise:
;; i. When m^2 > n, there are 0 ways to factor n using numbers no
;; smaller than m.
;; ii. When n is not divisible by m, then the number of ways to factor
;; n using numbers no smaller than m is the same as the number of ways
;; to factor n using numbers no smaller than m + 1 (because you are
;; using the same set of numbers to factor except you are leaving out
;; m, which was not a divisor of n anyways).
;; iii. When m^2 <= n, there is at least one way to factor n using
;; numbers no smaller than m (m and n/m).
(defn ways-to-factor-using-no-smaller-than [n m]
  (loop [acc 0
         x n
         y m]
       (cond (> (math/expt y 2) x) acc
             (zero? (mod x y))
                    (let [sub-factors (ways-to-factor-using-no-smaller-than (/ x y) y)]
                              (println (str x " divides " y ", calculate factors of " (/ x y)))
                              (recur (+ (inc acc) sub-factors) x (inc y)))
          :else (recur acc x (inc y)))))

(defn ways-to-factor [n]
  (ways-to-factor-using-no-smaller-than n 2))

;; Exercise 4.15
;; (This is probably over-repetitious)
(defn max-weighings [coll]
  "Returns the number of weighings to determine which coin is fake (exercise 4.15)"
  (loop [acc 0
         curr_coll coll]
    (cond (= 1 (count curr_coll)) acc
          (even? (count curr_coll))
          (let [mid (/ (count curr_coll) 2)
                left (take mid curr_coll)
                right (drop mid curr_coll)
                lweight (reduce + left)
                rweight (reduce + right)]
            (prn left right)
            (recur (inc acc) (if (< lweight rweight) left
                                 right)))
          :else
          (let [[head & tail] curr_coll
                mid (/ (count tail) 2)
                left (take mid tail)
                right (drop mid tail)
                lweight (reduce + left)
                rweight (reduce + right)]
            (prn left right)
            (recur (inc acc) (if (< lweight rweight) left
                                 right))))))

;; TODO: This is NOT RIGHT, cannot always partition by 3, need to
;; determine if modulus is 0 and if so then partition by (/ coll_sz 3;)
(defn max-weighings-thirds [coll]
  (loop [acc 0
         curr_coll coll]
    (Thread/sleep 100)
    (let [sz (count curr_coll)
          r (mod sz 3)
          sum #(reduce + %)]
      (println (str "r: " r))
      (cond (or (= 1 sz) (nil? curr_coll)) acc
            (zero? r) (let [psize (/ sz 3)
                            [p1 p2 p3 :as all] (partition psize curr_coll)]
                        (println "Divisible by 3")
                        (prn curr_coll)
                        (prn p1 p2 p3)
                        (cond (> (sum p1) (sum p2)) (recur (inc acc) p2)
                              (< (sum p1) (sum p2)) (recur (inc acc) p1)
                              :else (recur (inc acc) p3)))
            :else (let [psize (if (< (/ (- sz r) 3) 3) 1
                                  (/ (- sz r) 3)) ;; Always divisible by 3
                        [p1 p2] (partition psize (take (- sz r) curr_coll))
                        ;; This becomes a mess when the piles are
                        ;; smaller than 3...
                        p3 (drop (* 2 psize) curr_coll)]
                    (println "Not divisible by three")
                    (println "psize was: " (str psize))
                    (prn curr_coll)
                    (prn p1 p2 p3)
                    (cond (> (sum p1) (sum p2)) (recur (inc acc) p2)
                          (< (sum p1) (sum p2)) (recur (inc acc) p1)
                          :else (recur (inc acc) p3)))))))

;; Exercise 4.18
;; This doesn't work as it just keeps computing differences of numbers
;; which never have a difference of 1 (returns Rational types)
(defn sum-ints [a b]
  (if (= 1 (- b a)) 0
      (let [interval (- b a)
            mid (/ interval 2)
            a1 a
            b1 mid
            a2 (inc mid)
            b2 b]
        (prn a1 b1 a2 b2)
        (+ (sum-ints a1 b1) (sum-ints a2 b2)))))

