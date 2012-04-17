(ns euler.probs1-10
  (:require [euler.common :as util]
            [clojure.math.numeric-tower :as cmath]))

;; problem 1
(def problem1
  "Sum the multiples of 3 or 5 under 1000"
  (reduce + (filter
    #(or
       (= 0 (rem % 3))
       (= 0 (rem % 5)))
  (range 1000))))



;; problem 2
(defn fibs []
  (map first
       (iterate
         (fn [[a b]]
           [b (+ a b)]) [0 1])))

(defn problem2 [biggest]
  "sum even fibbonacci numbers below [biggest]"
  (reduce +
  (filter #(= 0 (rem % 2))
  (take-while #(< % biggest)
              (fibs)))))

(problem2 4000000)


;; problem 3

(def problem3
  (apply max (util/prime-factors 600851475143)))


;; problem 4
(defn palindrome? [input-str]
  (= (seq input-str) (reverse input-str)))


(def problem4
  (apply max
  (for [x (range 999 99 -1) y (range 999 99 -1)
        :when (palindrome? (str (* x y)))] (* x y))))



;; problem 5

(defn aggregate-factors [upper]
  (map frequencies
       (for [x (range 2 (+ 1 upper))]
         (util/prime-factors x))))


(defn find-prime-factor-count[upper]
  (apply merge-with #(max %1 %2) (aggregate-factors upper)))

(defn build-factor-list[upper]
  (map (fn [[x y]] (cmath/expt x y))
       (seq (find-prime-factor-count upper))))


(def problem5
  (apply * (build-factor-list 20)))



;; problem 6

(defn square-of-sum [x]
  (let [sumx (reduce + (util/rangeb x))]
    (* sumx sumx)))

(defn sum-of-squares [x]
  (let [squarex (pmap #(* % %) (util/rangeb x))]
    (reduce + squarex)))

(defn problem6 [x]
  (- (square-of-sum x) (sum-of-squares x)))

(problem6 100)


;; problem 7

(defn next-prime [start]
  (let [target (inc start)]
    (if (util/prime? target) target
        (next-prime target))))

(def problem7
  (nth (iterate next-prime 2) 10000))



;; problem 8

(def longstring
  (str
"73167176531330624919225119674426574742355349194934"
"96983520312774506326239578318016984801869478851843"
"85861560789112949495459501737958331952853208805511"
"12540698747158523863050715693290963295227443043557"
"66896648950445244523161731856403098711121722383113"
"62229893423380308135336276614282806444486645238749"
"30358907296290491560440772390713810515859307960866"
"70172427121883998797908792274921901699720888093776"
"65727333001053367881220235421809751254540594752243"
"52584907711670556013604839586446706324415722155397"
"53697817977846174064955149290862569321978468622482"
"83972241375657056057490261407972968652414535100474"
"82166370484403199890008895243450658541227588666881"
"16427171479924442928230863465674813919123162824586"
"17866458359124566529476545682848912883142607690042"
"24219022671055626321111109370544217506941658960408"
"07198403850962455444362981230987879927244284909188"
"84580156166097919133875499200524063689912560717606"
"05886116467109405077541002256983155200055935729725"
"71636269561882670428252483600823257530420752963450"))

(defn get-by-fives [str]
  (partition 5 1 str))

(defn convert-chars-to-nums [chars]
  (map #(Integer/parseInt (str %)) chars))

(defn get-products []
  (map #(reduce * %)
     (map #(convert-chars-to-nums %)
          (get-by-fives longstring))))

(def problem8
  (apply max (get-products)))


;; problem 9

(def problem9
  (for [a (util/rangeb 1000)
        b (util/rangeb 1000)
        c (util/rangeb 1000)
        :when (and (< a b c)
                   (= 1000 (+ a b c))
                   (= (+ (* a a) (* b b))
                      (* c c)))]
    (* a b c)))



;; problem 10

(def squares  (into [] (for [x (range 1415)] (* x x))))

(defn least-square [tested]
  (- (count
  (for [nums squares
        :while (< nums tested)] nums )) 1))

(defn try-primes [base primes]
  (for [x primes :while (not (=  0 (rem base x)))]  x))

(defn prime2? [base primes]
  (let [biggestProblem (least-square base)]
  (let [test-vals (filter #(<= % (/ base biggestProblem)) primes)]
  (= test-vals (try-primes base test-vals)))))

(defn add-prime [primes start]
  (let [target (inc start)]
    (if (prime2? target primes)
      (conj primes target)
      (add-prime primes target) )))

(defn build-prime [primes]
  (if (< 20000000 (last primes))
         (butlast primes)
  (recur (add-prime primes
            (last primes)))))

(defn problem10 []
  (time
   (reduce + (build-prime [2]))))
