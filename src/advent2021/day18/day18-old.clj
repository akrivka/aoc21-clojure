(ns advent2021.day18
  (:require [clojure.string :as str]))

(def main-input (slurp "src/advent2021/day18/day18.in"))

(def test-input (slurp "src/advent2021/day18/day18.example.in"))

(def example (read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map read-string)))

(defn convert' [i form]
  (if (vector? form)
    (mapcat (partial convert' (inc i)) form)
    [{:value form
      :level i}]))

(defn convert [tree-forms]
  (convert' 0 tree-forms))

(defn unconvert [seq-forms]
  (reduce
   (fn [{:keys [agg cur-level]}
        {:keys [value level]}]
     (let [diff (- level cur-level)]
       {:agg (apply str agg
                    (flatten
                     
                     (cond
                       (pos? diff) [(repeat diff "[") value]
                       (zero? diff) ["," value]
                       (neg? diff) [(repeat (- 0 diff) "],") value])))
        :cur-level level}))
   {:agg ""
    :cur-level 0}
   seq-forms))

(def converted-example (convert example))

(unconvert converted-example)

(defn explode [seq-forms]
  (loop [[{l1 :level
           v1 :value :as n1}
          {l2 :level
           v2 :value :as n2} & rem-queue
          :as queue]    seq-forms
         finished                   []]
    (cond
      (nil? n2)
      finished
      (and (= l1 l2) (> l1 4))
      (explode (concat
                (if (not-empty finished) (update-in (vec finished) [(dec (count finished)) :value] + v1) finished)
                [{:value 0 :level (dec l1)}]
                (if (not-empty rem-queue) (update-in (vec rem-queue) [0 :value] + v2) rem-queue)))
      :else
      (recur (rest queue) (conj finished n1)))))

(explode converted-example)

(defn sneduce [[left right] level]
  (let [level+ (inc level)
        [] (sneduce left level+)]

    (sneduce right level+)))

(sneduce example 0)

(defn part1 [input]
  (let [_ (parse-input input)]
    _))

(defn part2 [input])

(comment
  (part1 test-input))

(println
 "Day 18\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))