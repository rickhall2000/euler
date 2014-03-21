(ns euler.probs26-50
  (:require [euler.common :as util]
            [clojure.math.numeric-tower :as cmath]))

;; problem 26
(defn remainders [x]
  (loop [q 10 acc []]
    (let [r (rem q x)]
      (cond
       (zero? r) nil
       (util/index-of r acc) (vector x
                                (- (count acc)
                                   (util/index-of r acc)))
       :else (recur (* r 10) (conj acc r))))))

(defn problem26 [upper-bound]
  (->> (range 1 upper-bound)
       (map remainders)
       (remove nil?)
       (sort-by #(second %))
       (reverse)
       (take 1)))

;; problem 27
(defn apply-quadratic [b c n]
  (+ (* n n) (* n b) c))

(defn problem-27 [dim]
  (take 1
        (reverse
         (sort-by (fn [x] (second x))
                  (for [a (range (* -1 dim) dim)
                        b (range (* -1 dim) dim)]
                    (vector (* a b)
                            (count (take-while util/prime?
                                               (map #(apply-quadratic a b %)
                                                    (iterate inc 0)))) )) ))) )


;; problem 29
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


(defn char-to-power [char power]
  (let [num (Integer/parseInt (str char))]
    (cmath/expt num power)))

(defn sum-digits-to-nth [num power]
  (let [each (seq (str num))]
    (reduce +
            (map (fn [digit] (char-to-power digit power))
                 each))))

(defn problem30 [power-to-use max-to-test]
  (apply +
  (for [a (range 2 max-to-test)
        :when (= a (sum-digits-to-nth a power-to-use))] a)))

(defn series-n-to-nth [max]
  (reduce +
          (for [n (util/rangeb max)]
            (cmath/expt (bigint n) n))))

(defn problem48 [max]
  (apply str
         (reverse
          (take 10 (reverse
                    (seq (str (series-n-to-nth max))))))))
