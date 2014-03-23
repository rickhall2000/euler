(ns euler.prob26-50-test
  (:use clojure.test
        euler.probs26-50))


(deftest prob26-test
  (testing "the number below 10 with the longest cycle is 7 with 6"
    (is (= [7 6] (first (problem26 10))))))

(deftest prob28-test
  (testing "the spiral diaganals of a 5x5 should add up to 101"
    (is (= 101 (problem28 5)))))

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

(deftest series-n-to-nth-test
  (testing "sum of 1^1 .. 10^10 = 10405071317"
    (is (= 10405071317 (series-n-to-nth 10)))))

(deftest prob48-test
  (testing "last 10 digits of series 10^10 = 0405071317"
    (is (= "0405071317" (problem48 10)))))
