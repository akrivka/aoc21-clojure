(ns advent2021.day6
  (:require [advent2021.util :refer [node-slurp]]
            [clojure.string :as str]))

(def main-input
  (node-slurp "src/advent2021/day6/day6.in"))

(def test-input
  (node-slurp "src/advent2021/day6/day6.test.in"))

(defn parse-input [input]
  (map js/parseInt (str/split input #",")))

(defn get-next-fish [fish]
  (mapcat #(if (> % 0)
             [(dec %)]
             [6 8]) fish))

(defn simulate [input ndays]
  (let [initial-fish (parse-input input)
        final-fish ((apply comp (repeat ndays get-next-fish)) initial-fish)]
    (count final-fish)))

(def f
  (memoize (fn [k] (if (<= k 0)
                     1
                     (+ (f (- k 7)) (f (- k 9)))))))

(defn simulate-v2 [input ndays]
  (let [initial-fish (parse-input input)]
    (reduce #(+ %1 ((memoize f) (- ndays %2))) 0 initial-fish)))

(js/console.log
   "Day 6\n"
   "- Part 1: " (simulate-v2 main-input 150) "\n"
   "- Part 2: " (simulate-v2 main-input 256))