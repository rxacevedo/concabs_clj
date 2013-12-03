(ns concabs.ch6
  (:gen-class)
  (:use [cemerick.pomegranate :only (add-dependencies)]))

;; Chapter 6 - Compound Data and Data Abstraction

;; Game state ADT

(defn index [coll]
  "Indexes a coll (thanks, The Joy of Clojure!"
  (cond (map? coll) (seq coll)
        (set? coll) (map vector coll coll)
        :else (map vector (iterate inc 0) coll)))

(defn make-game-state [coll]
  "Creates a game-state with the supplied number of piles"
  (vec (map #(vec (repeat % :c)) coll)))


(defn size-of-pile [game-state pile-number]
  "Returns the size of pile pile-number in game-state."
   (count (nth game-state pile-number)))

;; TODO: Ensure pile exists
(defn remove-coins-from-pile [game-state pile-number num-coins]
  "Acquires position of pile via index (pile-number) and returns
   a subvec from 0 to (count - num-coins) assoc'd to game-state."
  (let [pile (nth game-state pile-number)
        sz (count pile)]
    (if (> num-coins sz)
      (throw (Exception. (str "Not enough coins in pile " pile-number " to remove " num-coins " coins.")))
      (let [new-sz (- sz num-coins)]
        (assoc game-state pile-number (subvec pile 0 new-sz))))))

(defn info [game-state]
  "Returns the status of game-state."
  (str "Current game state: " game-state))

;;; Game

(defn over? [game-state]
  "Returns true if all colls/piles are empty (game is over)."
  (reduce #(and %1 %2) (map empty? game-state)))

(defn human-move
  ([game-state]
     (println (info game-state))
     (println "Please enter the pile and number of coins: ")
     (let [[pile-number num-coins] (map #(Integer/parseInt (str %)) (re-seq #"[0-9]" (read-line)))]
       (human-move game-state pile-number num-coins)))
  ([game-state pile-number num-coins]
     (remove-coins-from-pile game-state pile-number num-coins)))

(defn computer-move [game-state]
  ;; (let [[p0 p1] :as colls game-state]
  ;;   (if (< (count p0) num-coins) (throw (Ex))))
  )

(defn play-with-turns [game-state player]
  (info game-state)
  (cond (over? game-state) nil
        (= player :player) (play-with-turns (human-move game-state))
        (= player :computer) (play-with-turns (computer-move game-state))
        :else (throw (Exception. "Unidentified player."))))
