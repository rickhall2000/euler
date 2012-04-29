(ns euler.common
  (:require [clj-time.core :as time ]))


(defmacro rangeb [last]
  (list 'range 1 (list '+ 1 last)))


(defn number-to-char-sequence [number]
  (seq (str number)))





(defn all-divisors [n]
  (for [i (rangeb n) :when (zero? (rem n i))]
    i))

(defn triangle [n]
  (loop [c 0 n n]
    (if (zero? n)
    c
    (recur (+ n c) (- n 1)))))

(defn triangle [^Integer n]
  (loop [c 0 n n]
    (if (zero? n)
    c
    (recur (+ n c) (- n 1)))))



(defn tri [n]
  (/ (* n (+ n 1)) 2))

(defn pent [n]
  (/ (* n (- (* 3 n) 1)) 2))

(defn hex [n]
  (* n (- (* 2 n) 1)))

(defn tri? [n]
  (* 2 (/ n (- n 1) )))


(defmacro foreach [[sym coll] & body]
  `(loop [coll# ~coll]
     (when-let [[~sym & xs#] (seq coll#)]
       ~@body
       (recur xs#))))

(require '(clojure [string :as str]
                   [walk :as walk]))

(defmacro reverse-it
  [form]
  (walk/postwalk #(if (symbol? %)
                    (symbol (str/reverse (name %)))
                    %)
                 form))
;; prime/factor functions


(defn lazy-factor [base]
  (for [x (rangeb base)
     :when (= 0 (rem base x))]
     x ))

(defn prime? [base]
  (and (> base 1)
  (= base (nth (lazy-factor base) 1))))

(defn factor [base]
  (let [first (nth (lazy-factor base) 1) ]
    (list first (/ base first))))

(defn factor-tree [base]
  (tree-seq (complement prime?) factor base))

(defn prime-factors [base]
  (filter #(prime? %)  (factor-tree base)))
