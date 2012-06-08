(ns euler.probs51-75
  (:require [euler.common :as util]
            [clojure.string :as strings]))


;; problem 67
(defn convert-row-to-nums [row]
  (map (fn [x] (Integer/parseInt x)) row))

(defn problem67 []
  (let [first-read (with-open [rdr (clojure.java.io/reader "triangle.txt")]
                     (reduce conj [] (line-seq rdr)))
        row-vecs (map (fn [x] (strings/split x #" ")) first-read)
        triangle (map convert-row-to-nums row-vecs)]
    (first (reduce util/combine-rows (reverse triangle)))))
