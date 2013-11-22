(ns concabs.ch5-test
  (:require [clojure.test :refer :all]
            [concabs.ch5  :refer :all]))

(deftest combine-tests
  (testing "Checks to ensure that all combinators are working."
    (is (= 10 (combine-1 + 2 5 0)))
    (is (= 100 (combine-2 + 2 50 0)))
    (is (= 1000 (combine-3 + 2 500 0)))
    (is (= 10000 (combine-4 + 2 5000 0)))
    (is (= 128 (combine-1 * 2 7 1)))
    (is (= 256 (combine-2 * 2 8 1)))
    (is (= 512 (combine-3 * 2 9 1)))
    (is (= 1024 (combine-4 * 2 10 1)))))

(deftest verifier-tests
  (testing "Checks to ensure that all verifiers are working."
    (is (= true (check-isbn 8090273416)))
    (is (= true (check-upc 663595981057)))
    (is (= true (check-credit-card 4111111111111111)))
    (is (= true (check-pmo-serial 48077462766)))))
