(ns advent2021.day13
  (:require [advent2021.util]
            [clojure.string :as str]))

(def main-input
  (slurp "src/advent2021/day13/day13.in"))

(def test-input
  (slurp "src/advent2021/day13/day13.test.in"))

(defn parse-input [input]
  (let [[dots-raw folds-raw] (str/split input #"\n\n")
        dots (->> dots-raw
                  (str/split-lines)
                  (map #(str/split % #","))
                  (map #(map parse-long %))
                  (map vec)
                  (set))
        folds (->> folds-raw
                   (str/split-lines)
                   (map #(let [[_ dir coord] (re-find #"(?<=fold along )(\w)=(\d+)" %)]
                           [(keyword dir) (parse-long coord)])))]
    [dots folds]))

(def dir-map {:x 0 :y 1})

(defn fold [dots [dir coord]]
  (into #{}
        (map #(let [ci (dir-map dir)
                    sc (get % ci)]
                (if (> sc coord)
                  (assoc % ci (- (* 2 coord) sc))
                  %))) dots))

(defn part1 [input]
  (let [[dots folds] (parse-input input)
        first-fold (fold dots (first folds))]
    (count first-fold)))

(defn print-paper [dots]
  (let [[[mx' _] [_ my']] (for [i [0 1]]
                   (apply max-key #(get % i) dots))
        grid (for [y (range (inc my'))]
               (for [x (range (inc mx'))]
                 (if (dots [x y]) "#" ".")))]
    (println (str/join "\n" (map #(apply str %) grid)))))

(defn part2 [input]
    (let [[dots folds] (parse-input input)
          result (reduce fold dots folds)]
      (print-paper result)))

(comment
  (part2 main-input)
)

(prn
 "Day 13\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))