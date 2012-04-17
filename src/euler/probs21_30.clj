(ns euler.probs21-30
  (:require [euler.common :as util]
            [clojure.math.numeric-tower :as cmath]))

;; problem 21

(def num-pairs
  (apply conj {}
         (for [n (util/rangeb 10000)]
           {n (apply + (util/all-divisors n))})))


(def problem21
  (for [a num-pairs
        :when (and (= (first a)
                      (get num-pairs (first (rest a))))
                   (not (= (first a) (first (rest a)))))]
    (first a)))


(apply + prob21)


;; problem 25

(defn fibs []
  (map first
       (iterate
        (fn [[a b]]
          [b (+ a b)]) [0N 1N])))

(defn prob2 [biggest]
  "sum even fibbonacci numbers below [biggest]"
  (reduce +
          (filter #(= 0 (rem % 2))
                  (take-while #(< % biggest)
                              (fibs)))))



(take 1 (filter #(= 1000
                    (count (str %))) (fibs)))

(count (take-while #(> 1000 (count (str %)))
            (fibs)))


;; problem 29
(def problem29
  (let [max 101N]
  (count
   (into #{}
         (for [a (range 2N max)
               b (range 2N max)]
           (cmath/expt a b))))))

;; problem 30
(def max (* 6 (cmath/expt 9 5)))

(defn sum-digs-to-5th [n]
  (let [each (seq (str n))]
    (reduce +
            (map #(cmath/expt (Integer/parseInt
                        (str %)) 5) each))))

(sum-digs-to-5th 1634)

(apply +
       (for [i (range 2 max)
             :when (= i (sum-digs-to-5th i))]
         i))
