(ns concabs.ch1
  (:gen-class))

(defn my-favorite-things [first & rest]
  "Print some stuff"
  (let [unimportant (clojure.string/join ", " rest)]
    (println (str "My favorite thing is: " first))
    (println (str "The rest of my things are: " unimportant))))
