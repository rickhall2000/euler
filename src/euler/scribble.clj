(ns euler.scribble
(:require [euler.common :as util]
            [clojure.math.numeric-tower :as cmath]))


#_(defn prime? [base]
  (and (not (contains? seive base) )
  (= base (nth (lazy-factor base) 1))))


#_(defn next-prime [start]
  (let [target (inc start)]
  (if (prime? target) target
              (next-prime target))))

#_(def squares  (into [] (for [x (range 1415)] (* x x))))


#_(defn least-square [tested]
  (- (count
  (for [nums squares
        :while (< nums tested)] nums )) 1))

#_(defn try-primes [base primes]
  (for [x primes :while (not (=  0 (rem base x)))]  x))


#_(defn prime2? [base primes]
  (and (not (contains? sieve base))
  (let [test-vals (filter #(<= % (/ base 2)) primes)]
  (= test-vals (try-primes base test-vals)))))


#_(defn add-prime [primes start]
  (let [target (inc start)]
    (if (prime2? target primes)
      (conj primes target)
      (add-prime primes target) )))



#_(defn build-prime [primes]
  (if (< 2000000 (last primes))
         (butlast primes)
  (recur (add-prime primes
            (last primes)))))

#_(defn problem10 []
  (time
(reduce + (build-prime [2]))))

#_(problem10)

;;"Elapsed time: 1.0495164543446E7 msecs"
;;142913828922

;; for 20k
;;"Elapsed time: 2784.180758 msecs"
;;21171191


#_(def *limit* 2000000)

#_(defn build-mults [base]
  (take-while (partial > *limit*) (iterate (partial + base) (+ base base) ) ))

#_(defn not-prime [prior newprime]
  (into prior (build-mults newprime)))

#_(def sieve
(-> (not-prime #{1} 2)
    (#(not-prime % 3))
    (#(not-prime % 5))
    (#(not-prime % 7))
    (#(not-prime % 11))
    (#(not-prime % 13))
    (#(not-prime % 17))
    (#(not-prime % 19))
    (#(not-prime % 23))
    (#(not-prime % 29))
    (#(not-prime % 31))
    (#(not-prime % 37))
    (#(not-prime % 41))
    (#(not-prime % 43))
    (#(not-prime % 47))
    (#(not-prime % 53))
    (#(not-prime % 59))
    (#(not-prime % 61))
    (#(not-prime % 67))
    (#(not-prime % 71))
    (#(not-prime % 73))))

#_(defn problem10 []
  (time
  (reduce + (filter #(prime? %) (rangeb *limit*)))))


;; problem 10 - another try

#_(def squares  (into [] (for [x (range 1415)] (* x x))))

#_(defn least-square [tested]
  (- (count
  (for [nums squares
        :while (< nums tested)] nums )) 1))

#_(defn try-primes [base primes]
  (for [x primes :while (not (=  0 (rem base x)))]  x))

#_(defn prime2? [base primes]
  (let [biggestProblem (least-square base)]
  (let [test-vals (filter #(<= % (/ base biggestProblem)) primes)]
  (= test-vals (try-primes base test-vals)))))

#_(defn add-prime [primes start]
  (let [target (inc start)]
    (if (prime2? target primes)
      (conj primes target)
      (add-prime primes target) )))

#_(defn build-prime [primes]
  (if (< 20000000 (last primes))
         (butlast primes)
  (recur (add-prime primes
            (last primes)))))

#_(defn problem10 []
  (time
   (reduce + (build-prime [2]))))
