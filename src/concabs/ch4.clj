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
