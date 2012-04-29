(ns euler.common-test
  (:use clojure.test
        euler.common))


(deftest fib-term-test
  (testing "Making sure my fibbonacci sequence term 10 = 55"
    (is (= 55 (nth (fibs) 10)))))
