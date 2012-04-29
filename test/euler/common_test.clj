(ns euler.common-test
  (:use clojure.test
        euler.common))


(deftest fib-term-test
  (testing "Making sure my fibbonacci sequence term 10 = 55"
    (is (= 55 (nth (fibs) 10)))))

(deftest all-divisor-sum-test
  (testing "The sum of all divisors of 220 is 504"
    (is (= 504 (apply + (all-divisors 220))))))
