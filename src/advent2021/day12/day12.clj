(ns advent2021.day12
  (:require [clojure.string :as str]
            [advent2021.util :refer [occurances]]))

(def main-input
  (slurp "src/advent2021/day12/day12.in"))

(def test-input
  (slurp "src/advent2021/day12/day12.test1.in"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(map keyword (str/split % #"-")))
       (reduce (fn [adj-list [v1 v2]]
                 (-> adj-list
                     (update v1 conj v2)
                     (update v2 conj v1))) {})))

(defn is-big? [v]
  (let [vs (name v)]
    (= vs (str/upper-case vs))))

(defn explore [adj-list dest path]
  (let [current (peek path)]
    (if (= current dest)
      [path]
      (mapcat vec
              (for [n (current adj-list)
                    :when (or (is-big? n)
                              (not ((set path) n)))]
                (explore adj-list dest (conj path n)))))))

(defn part1 [input]
  (let [adj-list (parse-input input)
        paths  (explore adj-list :end [:start])]
    (count paths)))

(defn explore2 [adj-list dest path small-twice?]
  (let [current (peek path)]
    (if (= current dest)
      [path]
      (remove nil?
              (mapcat vec
                      (for [n (current adj-list)
                            :let [no (occurances path n)]
                            :when (not= n :start)]
                        (cond
                          (is-big? n) (explore2 adj-list dest (conj path n) small-twice?)
                          (= no 0)    (explore2 adj-list dest (conj path n) small-twice?)
                          (and (= no 1) (not small-twice?))    (explore2 adj-list dest (conj path n) true)
                          :else   nil)))))))


(defn part2 [input]
  (let [adj-list (parse-input input)
        paths  (explore2 adj-list :end [:start] false)]
    (count paths)))

(comment
  (part2 (slurp "src/advent2021/day12/day12.in"))
  
  )

(println
 "Day 12\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))