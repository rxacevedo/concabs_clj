(ns concabs.ch7
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

(defn add-to-end [coll elt]
  "Returns a coll with elt appended to it by recursively cdring down the initial coll."
  (if (seq coll)
    (cons (first coll) (add-to-end (rest coll) elt))
    (cons elt '())))

(defn rvrs [coll]
  "Reverse using add-to-end.
   O(n^2) aka horrible."
  (if (seq coll)
    (add-to-end (rvrs (rest coll)) (first coll))))

(defn my-rvrs [coll]
  "Better verison of rvrs that works by consing all elements in coll to another initially empty list.
   O(n) time."
  (loop [c coll
         acc '()]
    (if (seq c)
      (recur (rest c) (cons (first c) acc))
      acc)))

(defn foldl [seed]
  "Basically foldLeft from Scala, probably not as efficient."
  (fn [f coll]
    (loop [c coll
           acc seed]
      (if (seq c)
        (recur (rest c) (f acc (first c)))
        acc))))

(def my-sum #((foldl 0) + %))

(def my-factorial #((foldl 1) * (range 1 (inc %))))
