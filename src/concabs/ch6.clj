(ns concabs.ch6
  (:gen-class)
  (:use [cemerick.pomegranate :only (add-dependencies)]))

;; Chapter 6 - Compound Data and Data Abstraction

;; Game state ADT

(defn make-game-state [n m]
  [(vec (repeat n :c)) (vec (repeat m :c))])

(defn size-of-pile [game-state pile-number]
  ;; (println "In size-of-pile")
  (count (nth game-state pile-number)))

(defn remove-coins-from-pile [game-state num-coins pile-number]
  (let [[p0 p1] game-state
        sz (size-of-pile game-state pile-number)]
    ;; (println "In remove-coins")
    (if (> num-coins sz)
      (throw (Exception. (str "Not enough coins in pile " pile-number " to remove " num-coins " coins.")))
      (let [new-sz (- sz num-coins)]
        (cond (= 0 pile-number) [(subvec p0 0 new-sz) p1]
              (= 1 pile-number) [p0 (subvec p1 0 new-sz)]
              :else (throw (Exception. "Crap")))))))

(defn info [])

(defn play-with-turns [game-state player]
  (info game-state))

