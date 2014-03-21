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

(deftest indexed-test
  (testing "indexd function '(a b c d) shourd return ([0 a] [1 b]...)"
    (let [result '([0 a] [1 b] [2 c] [3 d])]
      (is (= result (indexed '(a b c d)))))))

(deftest letter-to-number-test
  (testing "passing capital letters should return their order in alphabet"
    (is  (= 1 (letter-to-number \A)))
    (is  (= 26 (letter-to-number \Z)))
    (is  (= 13 (letter-to-number \M)))
    (is  (= 4 (letter-to-number \D)))))

(deftest abundant-number-test
  (testing "12 is an abundant number"
    (is (abundant-number? 12))))

(deftest index-of-test
  (testing "index function should return nil when not found, first index when found"
    (is (nil? (index-of 3 [0 1 2])))
    (is (= 2 (index-of 3 [1 2 3 4])))
    (is (= 3 (index-of 3 [0 1 2 3 3 3 3])))))
