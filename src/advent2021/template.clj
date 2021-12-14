(ns advent2021.day6
  (:require [clojure.string :as str]))

(def main-input (slurp "src/advent2021/day6/day6.in"))

(def test-input (slurp "src/advent2021/day6/day6.test.in"))

(defn parse-input [input]
  (->> input))

(defn part1 [input]
  (let [_ (parse-input input)]
    _))

(defn part2 [input])

(comment
  (part1 test-input))

(println
 "Day 6\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))