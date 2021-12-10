(ns advent2021.day7
  (:require [advent2021.util :refer [node-slurp abs exp]]
            [clojure.string :as str]))

(def main-input
  (node-slurp "src/advent2021/day7/day7.in"))

(def test-input
  (node-slurp "src/advent2021/day7/day7.test.in"))

(defn parse-input [input]
  (map js/parseInt (str/split input #",")))

(defn get-fuel [crabs x]
  (reduce #(+ %1 (abs (- %2 x))) 0 crabs))

(defn part1 [input]
  (let [crabs (parse-input input)
        op-x (nth (sort crabs) (quot (count crabs) 2))]
    (get-fuel crabs op-x)))

(defn get-fuel2 [crabs x]
  (reduce #(let [d (abs (- %2 x))]
             (+ %1 (/ (* d (inc d)) 2))) 0 crabs))

(defn part2 [input]
  (let [crabs (parse-input input)
        fuels (map (fn [x] [x (get-fuel2 crabs x)]) (range (apply min crabs) (apply max crabs)))
        sorted-fuels (sort-by second fuels)
        [[_ min-fuel]] sorted-fuels]
    min-fuel))

(part2 main-input)

(comment)

(js/console.log

 "Day 7\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))
 