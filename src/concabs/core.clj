(ns concabs.core
  (:gen-class)
  (:require [concabs.ch1 :as ch1]
            [concabs.ch2 :as ch2]
            [concabs.ch3 :as ch3]
            [concabs.ch4 :as ch4]))

(defn -main
  [& args]
  (ch1/my-favorite-things "computers" "work" "cleaning")
  (println (str "Factorial of 10: " (ch2/factorial 5)))
  (println (str "Number of digits in 4563489:" (ch2/num-digits 4563489)))
  (println (str "3rd Fermat number: " (ch3/fermat-number 3)))
  (println (str "Verified signature: " (ch4/verify ch4/signature ch4/modulus)))
  (println (str "mod-expt works? " (and (= (ch4/mod-expt ch4/signature 3 ch4/modulus) ch4/gold-num) (= (ch4/mod-expt ch4/gold-num ch4/signing-exponent ch4/modulus) ch4/signature)))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
