(ns advent2021.day5
  (:require [advent2021.util :refer [node-slurp abs]]
            [clojure.string :as str]))

(def main-input (node-slurp "src/advent2021/day5/day5.in"))

(def test-input (node-slurp "src/advent2021/day5/day5.test.in"))

(defn parse-line [line]
  (map #(map js/parseInt (str/split % #",")) (str/split line #" -> ")))

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn expand-hv-line [[[x1 y1] [x2 y2]]]
  (assert (or (= x1 x2) (= y1 y2)))
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    [x y]))

(defn part1 [input]
  (let [lines (parse-input input)
        hv-lines (filter
                  (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2)))
                  lines)
        huge-array (->> hv-lines
                        (map expand-hv-line)
                        (apply concat))
        freq (frequencies huge-array)
        freq>=2 (keep (fn [[p f]] (when (>= f 2) p)) freq)]
    (count freq>=2)))

(defn expand-hvd-line [[[x1 y1 :as v1] [x2 y2 :as v2] :as line]]
  (assert (or (= x1 x2)
              (= y1 y2)
              (= (abs (- x1 x2)) (abs (- y1 y2)))))
  (if (or (= x1 x2) (= y1 y2))
    (expand-hv-line line)
    (let [n (abs (- x1 x2))
          v (map - v2 v1)
          vn (map #(/ % n) v)]
      (for [i (range (inc n))]
        (map + v1 (map #(* % i) vn))))))

(defn part2 [input]
  (let [lines (parse-input input)
        hvd-lines (filter
                   (fn [[[x1 y1] [x2 y2]]]
                     (or (= x1 x2)
                         (= y1 y2)
                         (= (abs (- x1 x2)) (abs (- y1 y2)))))
                   lines)
        huge-array (->> hvd-lines
                        (map expand-hvd-line)
                        (apply concat))
        freq (frequencies huge-array)
        freq>=2 (keep (fn [[p f]] (when (>= f 2) p)) freq)]
    (count freq>=2)))

(comment
  (part1 main-input))

(js/console.log
 "Day5: \n"
 "- Part 1: " (part1 main-input) "\n"
 "- Part 2: " (part2 main-input))