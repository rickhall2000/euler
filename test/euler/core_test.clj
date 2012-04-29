(ns euler.core-test
  (:use clojure.test
        euler.probs1-25))


(deftest prob1-test
  (testing "All of the multiples of five added together equal 23"
    (is (= 23 (problem1 10)))))

(deftest prob2-test
  (testing   "The sum of even fibonacci numbers below 89 is 44"
    (is (= 44 (problem2 89)))))

(deftest prob3-test
  (testing "The largest prime factor of 13195 is 29"
    (is (= 29 (problem3 13195)))))

(deftest prob4-test
  (testing "The largest palendrome that is a product of 2 2-digit nums is 9009"
    (is (= 9009 (problem4 99)))))

(deftest prob5-test
  (testing "The smallest number divisible by 1 - 10 is 2520"
    (is (= 2520 (problem5 10)))))

(deftest sum-of-squares-test
  (testing "The sum of the first 10 squares is 385"
    (is (= 385 (sum-of-squares 10)))))

(deftest square-of-sum-test
  (testing "Square of the sum of 1-10 is 3025"
    (is (= 3025 (square-of-sum 10)))))

(deftest prob6-test
  (testing "Square of sum - sum of square of 10 is 2620"
    (is (= 2640 (problem6 10)) )))

(deftest prob7-test
  (testing "The 6th prime is 13"
    (is (= 13 (problem7 6)) )))

;; problem 10 out for maintenance
#_(deftest prob10-test
  (testing "The sum of primes below 10 = 17"
    (is (= 17 (problem10 10)))))
