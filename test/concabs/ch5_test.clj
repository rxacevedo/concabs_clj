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

(deftest make-exponentiater-tests
  (testing "Checks to ensure that make-exponentiater is working."
    (is (= 25 ((make-exponentiater 2) 5)))
    (is (= 125 ((make-exponentiater 3) 5)))
    (is (= 25 (square 5)))
    (is (= 125 (cube 5)))))

(deftest make-repeated-version-of-tests
  (testing "Checks to ensure that make-repeated-version-of is working."
    (is (= 5 ((comp inc (make-repeated-version-of square)) 2 1)))
    (is (= 5 (fermat 1)))
    (is (= 4294967297 (fermat 5)))))

(deftest combine-down-tests
  (testing "Checks to ensure that all combine-down methods are working."
    (is (= 15 (combine-down + 5)))
    (is (= 120 (combine-down * 5)))
    (is (= 5050 ((combine-down-2 + 100) #(Long. %))))
    (is (= 3628800 ((combine-down-2 * 10) #(Long. %))))
    (is (= 55 ((combine-down-2 + 5) square)))
    (is (= 15 ((combine-down-3 + 5) #(Long. %) dec)))
    (is (= 120 ((combine-down-3 * 5) #(Long. %) dec)))
    (is (= 55 ((combine-down-3 + 5) square dec)))))

(deftest verifier-tests
  (testing "Checks to ensure that all verifiers are working."
    (is (= true (check-isbn 8090273416)))
    (is (= true (check-upc 663595981057)))
    (is (= true (check-credit-card 4111111111111111)))
    (is (= true (check-pmo-serial 48077462766)))))
