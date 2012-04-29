(ns euler.probs26-50
  (:require [euler.common :as util]
            [clojure.math.numeric-tower :as cmath]))


;; problem 29
#_(defn problem29 []
  (let [max 101N]
  (count
   (into #{}
         (for [a (range 2N (+ 1 max))
               b (range 2N max)]
           (cmath/expt a b))))))

(defn problem29 [max]
  (count
   (into #{}
         (for [a (range 2N (+ 1 max))
               b (range 2N (+ 1 max))]
           (cmath/expt a b)))))



;; problem 30
#_(def max (* 6 (cmath/expt 9 5)))

#_(defn sum-digs-to-5th [n]
  (let [each (seq (str n))]
    (reduce +
            (map #(cmath/expt (Integer/parseInt
                        (str %)) 5) each))))

#_(sum-digs-to-5th 1634)

#_(apply +
       (for [i (range 2 max)
             :when (= i (sum-digs-to-5th i))]
         i))
