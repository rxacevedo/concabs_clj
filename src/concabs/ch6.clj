;; Chapter 6 - Compound Data and Data Abstraction

;; Game state ADT

(defn make-game-state [n m]
  [(vec (repeat n :c)) (vec (repeat m :c))])

(defn size-of-pile [game-state pile-number]
  (count (nth game-state pile-number)))

(defn remove-coins-from-pile [game-state num-coins pile-number]
  (let [pile (nth game-state pile-number)]
    (if (> num-coins (size-of-pile game-state pile-number))
      (throw (Exception. (str "Not enough coins in pile " pile-number " to remove " num-coins " coins.")))
      (vec ()))))

(defn play-with-turns [game-state player]
  (info game-state))

