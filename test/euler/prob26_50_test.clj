(ns euler.prob26-50-test
  (:use clojure.test
        euler.probs26-50))

(deftest prob29-test
  (testing "there are 15 combinations a^b where a and b are > 2 and < 5"
    (is (= 15 (problem29 5)))))
