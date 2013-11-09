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
