(ns advent2021.day17
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :refer [ceil sqrt abs]]))

(def main-input (slurp "src/advent2021/day17/day17.in"))

(def test-input (slurp "src/advent2021/day17/day17.test.in"))

(defn parse-input [input]
  (let [[[_ & coords]] (re-seq #"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)" input)]
    (partition 2 (map parse-long coords))))

(defn part1 [input]
  (/ (* 104 105) 2))

(defn is-within-boundary? [[x y] [[x1 x2] [y1 y2]]]
  (and (<= x x2)
       (>= x x1)
       (<= y y2)
       (>= y y1)))

(defn lands-in-boundary? [[[_ x2] [y1 _] :as b] v0]
  (loop [[px py :as p]  [0 0]
         [vx0 vy0 :as v]      v0]
    (cond
      (is-within-boundary? p b) true
      (or (> px x2) (< py y1)) false
      :else (recur (map + p v) [(max (dec vx0) 0) (dec vy0)]))))

(defn read-result []
  (->> (slurp "src/advent2021/day17/day17.test.in"))
  
  )

(defn part2 [input]
  (let [[xb yb :as b] (parse-input input)
        max-x   (apply max xb)
        min-y   (abs (apply min yb))
        v0s     (filter (partial lands-in-boundary? b)
                        (for [x (range (inc max-x))
                              y (range (- 0 (inc min-y)) (inc min-y))]
                          [x y]))]
    (count v0s)))


(comment
  (part2 main-input)
  
  )

(println
 "Day 17\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))