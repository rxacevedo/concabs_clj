(ns concabs.core
  (:gen-class)
  (:require [concabs.ch1 :as ch1]
            [concabs.ch2 :as ch2]
            [concabs.ch3 :as ch3]))

(defn -main
  [& args]
  (ch1/my-favorite-things "computers" "work" "cleaning")
  (println (str "Factorial of 10: " (ch2/factorial 5)))
  (println (str "Number of digits in 4563489:" (ch2/num-digits 4563489)))
  (println (str "3rd Fermat number: " (ch3/fermat-number 3))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
