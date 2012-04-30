(ns euler.prob26-50-test
  (:use clojure.test
        euler.probs26-50))

(deftest prob29-test
  (testing "there are 15 combinations a^b where a and b are > 2 and < 5"
    (is (= 15 (problem29 5)))))

(deftest char-to-power-test
  (testing "the char 3 raised to the 3rd power should yield 27"
    (is (= 27 (char-to-power (first (seq "3")) 3)))))

(deftest sum-digits-to-nth-test
  (testing "the digits in 1634 to the 4th add up to 1634"
    (is (= 1634 (sum-digits-to-nth 1634 4)))))

(deftest prob30-test
  (testing "sum of numbers can be written as 4th power of digits"
    (is (= 19316 (problem30 4 9999)))))
