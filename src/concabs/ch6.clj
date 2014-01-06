(ns concabs.ch6
  (:gen-class))

;; Chapter 6 - Compound Data and Data Abstraction
;; Wow this game is boring.

;; Game state ADT methods

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

(defn remove-coins-from-pile [game-state pile-number num-coins]
  "Acquires position of pile via index (pile-number) and returns
   a subvec from 0 to (count - num-coins) assoc'd to game-state."
  (let [pile (nth game-state pile-number)
        sz (size-of-pile game-state pile-number)]
    (if (> num-coins sz)
      (throw (IndexOutOfBoundsException.
              (str "Not enough coins in pile " pile-number " to remove " num-coins " coins."))))
    (let [new-sz (- sz num-coins)]
      (assoc game-state pile-number (subvec pile 0 new-sz)))))

(defn info [game-state]
  "Returns the status of game-state, pprinting into StringWriter feels like a hack..."
  (let [sw (java.io.StringWriter.)]
    (binding [*out* sw]
      (clojure.pprint/pprint (index game-state)))
    (println (str "Current game state:\n" (.toString sw)))))

;; Game logic

(defn over? [game-state]
  "Returns true if all colls/piles are empty (game is over)."
  ;; (reduce #(and %1 %2) (map empty? game-state))
  (let [sizes (map #(size-of-pile game-state %) (range 0 (count game-state)))
        total (reduce + sizes)]
    (zero? total)))

(defn human-move
  ([game-state]
     "Called when it's a human player's turn - cider does not like this method and throws
      exceptions related to concurrency, methinks it has to do with the call to read-line."
     (println "Please enter the pile and number of coins: ")
     (let [input (read-line)
           nums (re-seq #"\d+" input)]
       (let [parsed (map #(Integer/parseInt %) nums)
             [pile-number num-coins] parsed]
         (if (= 2 (count parsed))
           (human-move game-state pile-number num-coins)
           (do
             (println "Error: please enter a valid (numeric) input string.")
             (recur game-state))))))
  ([game-state pile-number num-coins]
     (remove-coins-from-pile game-state pile-number num-coins)))

(defn computer-move
  ([game-state]
     "Performs a move for the computer by removing one coin from the first non-empty pile."
     (computer-move game-state 0))
  ([game-state pile-number]
     (if-not (zero? (count (nth game-state pile-number)))
       (do
         (println "Thinking...")
         (Thread/sleep 1000)
         (println)
         (remove-coins-from-pile game-state pile-number 1))
       (recur game-state (inc pile-number)))))

(defn play-with-turns [game-state player]
  "Recurs until the game is over."
  (info game-state)
  (if-not (over? game-state)
    (condp = player
      :human (recur (human-move game-state) :computer)
      :computer (recur (computer-move game-state) :human)
      (throw (Exception. "Unidentified player.")))
    (println (str (clojure.string/capitalize (name player)) " wins!"))))
