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
   O(n^2)."
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

;; Messing around
(defn repeatedly-fn [f]
  (fn [base times]
    (loop [acc base
           i times]
      (if (zero? i) acc
          (recur (f acc) (dec i))))))

(defn combine [f]
  (fn [base exponent seed]
    (loop [acc seed
           i exponent]
      (if (zero? i) acc
          (recur (f acc base) (dec i))))))

(defn square [x] (* x x))

(def my-sum #((foldl 0) + %))

(def my-factorial #((foldl 1) * (range 1 (inc %))))

(defn intrlv [c1 c2]
  "Interleaves two collections such that the first element is the car of c1."
  (if (seq c1)
    (cons (first c1) (intrlv c2 (rest c1)))
    c2))

;; (defn shffl [coll size]
;;   "Shuffles a collection by splitting it into halves and interleaving them using intrlv."
;;   (let [half (quot (inc size) 2)]
;;     (intrlv (take half coll) (drop half coll))))

(defn shffl [coll]
  "Shuffles a collection by splitting it into halves and interleaving them using intrlv."
  (let [half (quot (inc (count coll)) 2)]
    (intrlv (take half coll) (drop half coll))))

(defn mrg [c1 c2]
  "Merges two collections with the lesser car of the two cons'd to the recursive mrg of
   its cdr and the other list."
  (cond (not (seq c1)) c2
        (not (seq c2)) c1
        (< (first c1) (first c2)) (cons (first c1) (mrg (rest c1) c2))
        :else (cons (first c2) (mrg c1 (rest c2)))))

(defn mrg-sort [coll]
  "Lazy merge-sort that is actually beating clojure.core/sort (Arrays.sort) in this case,
   maybe because Java 1.7 uses timsort?"
  (if (seq (rest coll))
    (let [half (quot (inc (count coll)) 2)
          c1 (take half coll)
          c2 (drop half coll)]
      (lazy-cat (mrg-sort c1) (mrg-sort c2)))
    coll))

;; To plot in Incanter:
;; TODO: Wrap this in a method or use as a start for a package that
;; can be imported into other projects for easy empircal analysis.
;; (let [coll-sizes (map #(expt 2 %) (range 0 15))
;;                     methods ['mrg-sort 'sort]]                  
;;                 (-> (for [x methods
;;                           y coll-sizes
;;                           :let [z (binding [*out* (java.io.StringWriter.)]
;;                                     (time ((resolve x) (shffl (range 0 y))))
;;                                     (->> (.. *out* toString)
;;                                          (re-find #"\d+.\d+")
;;                                         Double/parseDouble))]] [x y z])
;;                     to-dataset
;;                     (with-data 
;;                       (view (line-chart :col-1 :col-2 
;;                                         :group-by :col-0 
;;                                         :x-label "coll-size" 
;;                                         :y-label "runtime (ms)" 
;;                                         :legend true)))))

(defn rvrs-2 [coll]
  (let [inner (fn [c acc]
                (if (seq c)
                  (recur (rest c) (cons (first c) acc))
                  acc))]
    (inner coll '())))

(declare odd-part)

(defn even-part [coll]
  (if (seq coll)
    (odd-part (rest coll))
    coll))

(defn odd-part [coll]
  (if (seq coll)
    (cons (first coll) (even-part (rest coll)))
    coll))

;; Excercise 7.15
;; This same method can be used to solve 7.19 by simply not repeating
;; the elements (do not pass to gen-prize-list)
(defn count-combos [prize-list amount]
  "Returns the number of possible combinations of prizes that one can buy
   given a list of prize values and a number of tickets."
  (cond (not (seq prize-list)) 0
        (< amount 0) 0
        (zero? amount) 1
        :else (+ (count-combos prize-list (- amount (first prize-list)))
                 (count-combos (rest prize-list) amount))))

;; LOL why did I do this???
;; (defn gen-prize-list [hi prize-counts]
;;   "Returns a list of prizes in a format that is appropriate for count-combos."
;;   (let [tuples (loop [i hi
;;                       p prize-counts
;;                       coll []]
;;                  (if (seq p)
;;                    (recur (dec i) (rest p) (conj coll [i (first p)]))
;;                    coll))]
;;     (flatten (map #(let [[a b] %]
;;                      (repeat b a)) tuples))))

(defn gen-prize-list
  ([prize-counts]
     "Returns a list of prizes in a format that is appropriate for count-combos."
     (gen-prize-list (count prize-counts) prize-counts))
  ([hi prize-counts]
     "Returns a list of prizes in a format that is appropriate for count-combos."
     (mapcat (fn [a b] (repeat b a)) (iterate dec hi) prize-counts)))

;; Ex 7.18
;; TODO: This is wrong
;; (defn count-combos-no-greater-than-max [prize-list amount]
;;   (for [amt (take amount (iterate dec amount))]
;;     (count-combos (gen-prize-list amt prize-list) amt)))
