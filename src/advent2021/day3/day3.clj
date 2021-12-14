(ns advent2021.day3
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :refer [expt]]))

(def input
  (->> (slurp "src/advent2021/day3/day3.in")
       (str/split-lines)))

(def test-input
  (->> (slurp "src/advent2021/day3/day3.test.in")
       (str/split-lines)))

(defn get-digits [s]
  (map #(Character/digit % 10) (seq s)))

(defn binary->decimal-logical [bs]
  (second 
    (reduce
      (fn [[e r] b]
        [(inc e) 
         (if b 
          (+ r (expt 2 e))
          r)])
      [0 0]
      (reverse bs))))

(defn binary->logical [s]
  (map #(= % \1) s))

(defn binary->decimal [s]
  (->> s
    (binary->logical)
    (binary->decimal-logical)))

(defn get-most-common-bits [input]
  (let [n (count input)
          freq (reduce 
                #(->> %2
                  (get-digits)
                  (map + %1)) 
                (repeat (count (first input)) 0) 
                input)]
    (map #(>= % (quot n 2)) freq)))

(defn part1 
  ([] (part1 input))
  ([input]
    (let [gamma-bits (get-most-common-bits input)
          epsilon-bits (map not gamma-bits)]
          (->> [gamma-bits epsilon-bits]
            (map binary->decimal-logical)
            (apply *)))))

(defn get-rating [input bit-criterion]
  (loop [p 0 v input]
    (let [freq (frequencies (map #(nth % p) v))
          sig-bit (bit-criterion freq)
          nv (filter #(= (nth % p) sig-bit) v)]
          (if (= (count nv) 1)
            (first nv)
            (recur (inc p) nv)))))

(defn part2 
  ([] (part2 input))
  ([input]
    (let [ox-rating (get-rating input #(if (>= (get % \1) (get % \0)) \1 \0))
          co2-rating (get-rating input #(if (<= (get % \0) (get % \1)) \0 \1))]
      (->> [ox-rating co2-rating]
        (map binary->decimal)
        (apply *)))))

(binary->decimal "101101")

(comment 
  (part1 test-input)
  (part2)
  )