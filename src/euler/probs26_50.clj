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

;; problem 28
(defn get-row [n]
  (- (* 4 (* n n))
     (* 6 (- n 1))))

(defn problem28 [side]
  (->> (range 2 (inc side))
       (filter odd?)
       (map get-row)
       (reduce + 1)))

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

;; problem 31
(defn problem31 [tgt denoms]
  (cond
   (< tgt 0) 0
   (zero? tgt) 1
   (empty? denoms) 0
   :else (+ (problem31 (- tgt (first denoms)) denoms)
      (problem31 tgt (rest denoms)))))

#_(problem31 200 [200 100 50 20 10 5 2 1])

;; problem 32
(defn pandigital? [& xs]
  (let [cmb (apply str xs)]
    (and (= (count (set cmb)) (count cmb) 9)
         (not (util/index-of \0 cmb)))))

(defn problem32 []
  (reduce +
          (reduce conj #{}
                  (for [i (range 100000N) j (range 1000)
                        :let [z (* i j)]
                        :when (pandigital? i j z)]
                    z))))

;; problem 33
(defn curious? [a b]
  (let [a1 (read-string (subs (str a) 0 1))
        a2 (read-string (subs (str a) 1 2))
        b1 (read-string (subs (str b) 0 1))
        b2 (read-string (subs (str b) 1 2))]
    (and (= a2 b1) (not (= a2 b2)) (= (/ a b) (/ a1 b2)))))

(defn problem33 []
  (reduce *
          (for [a (range 11 100)
                b (range 11 100)
                :when (and (> b a)
                           (> (mod b 10) 0)
                           (curious? a b))]
             (/ a b))))

;; problem 34
(defn sum-of-facts [n]
  (let [digits
        (map (comp read-string str) (seq (str n)))]
    (reduce +
            (map util/factorial digits))))

(defn problem-34 []
  (let [upper-bound (util/factorial 10)]
    (->> (iterate inc 3)
         (take-while #(< % upper-bound))
         (filter (fn [x] (= x (sum-of-facts x))))
         (reduce +))))

;; problem 35
(defn next-circle [n]
  (let [as-str (str n)
        circle (str (apply str (rest as-str)) (first as-str))]
    (read-string circle)))

(defn get-circles [n]
  (let [len (count (str n))]
    (take len
          (iterate next-circle n))))

(defn all-odd? [n]
  (let [digits (map (comp read-string str) (seq (str n)))]
    (every? odd? digits)))


(defn problem-35 [upper-bound]
  (let [possibles
        (->> (range upper-bound)
             (filter all-odd?)
             (filter util/prime?)
             (reduce conj #{2}))]
    (count
     (filter (fn [x] (every? possibles x))
             (map get-circles possibles)))))

(defn palendrome? [n]
  (let [num (str n)]
    (= (seq num) (reverse num))))

(defn problem36 []
  (->> (range 1000000)
       (filter odd?)
       (filter (fn [x] (palendrome? (util/binary x))))
       (filter palendrome?)
       (reduce +)))



;; problem 48
(defn series-n-to-nth [max]
  (reduce +
          (for [n (util/rangeb max)]
            (cmath/expt (bigint n) n))))

(defn problem48 [max]
  (apply str
         (reverse
          (take 10 (reverse
                    (seq (str (series-n-to-nth max))))))))
