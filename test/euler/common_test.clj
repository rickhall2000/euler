(ns euler.common-test
  (:use clojure.test
        euler.common))


(deftest fib-term-test
  (testing "Making sure my fibbonacci sequence term 10 = 55"
    (is (= 55 (nth (fibs) 10)))))

(deftest all-divisor-sum-test
  (testing "The sum of all divisors of 220 is 504"
    (is (= 504 (apply + (all-divisors 220))))))

(deftest pascal-row-test
  (testing "Passing in [1 1] to pascal-row should yield [1 2 1]"
    (is (= [1 2 1] (pascal-row [1 1])))))
