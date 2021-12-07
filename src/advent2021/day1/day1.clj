(ns advent2021.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (slurp "src/day1/input")
       (str/split-lines)
       (map read-string)))

(defn get-result [input]
  (second (reduce
           (fn [[prev result] next]
             [next (if (> next prev)
                     (inc result)
                     result)])
           [1000000000000 0]
           input)))

(comment
  (get-result input))