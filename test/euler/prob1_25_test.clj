(ns euler.prob1-25-test
  (:use clojure.test
        euler.probs1-25))


(deftest prob1-test
  (testing "All of the multiples of five added together equal 23"
    (is (= 23 (problem1 10)))))

(deftest prob2-test
  (testing   "The sum of even fibonacci numbers below 89 is 44"
    (is (= 44 (problem2 89)))))

(deftest problem2-actual
  (testing "testing actual result of problem 2 for refactoring"
    (is (= 4613732 (problem2 4000000)))))

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

(deftest probl15-test
  (testing "For a 2x2 grid, there are 6 routes from the top left to bottom right"
    (is (= 6 (problem15 2)))))

(deftest prob15-suplimental-test
  (testing "a 5x5 grid has 252 routes.  Adding because I need to refactor"
    (is (= 252 (problem15 5)))))

(deftest prob16-test
  (testing "The sum of the digits of 2^15 is 26"
    (is (= 26 (problem16 15)))))

(deftest prob16-actual
  (testing "Testing actual solution for refactoring"
    (is (= 1366 (problem16 1000)))))

(deftest prob18-test
  (testing "The sample triangle should add up to 23"
    (let [sample [[3] [7 4] [2 4 6] [8 5 9 3]]]
      (is (= 23 (problem18 sample))))))

(deftest prob20-test
  (testing "The sum of the digits in 10! is 27"
    (is (= 27 (problem20 10)))))

(deftest problem20-actual
  (testing "Testing actual solution of problem 20 for refactoring"
    (is (= 648 (problem20 100)))))

(deftest proper-factor-test
  (testing "The sum of the proper factors of 220 is 284"
    (is (= 284 (get-sum-of-proper-factors 220)))))

(deftest amicable-test
  (testing "220 and 284 are amicable numbers"
    (is (= 284 (get (make-factor-sum-pairs 220) 220 ) ))))

(deftest problem21-actual
  (testing "Checking 21 with the real answer cause its easier that way"
    (is (= 31626 (problem21 10000)))))

(deftest word-score-test
  (testing "The letters in COLIN should add up to 53 - prob 22"
    (is (= 53 (word-score "COLIN")))))

(deftest prob25-test
  (testing "The 12th term of fibbonacci sequence is the first in 3 digits"
    (is (= 12 (problem25 3)))))
