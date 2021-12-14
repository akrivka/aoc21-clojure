(ns advent2021.day8
  (:require [advent2021.util :refer [node-slurp single]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def main-input
  (node-slurp "src/advent2021/day8/day8.in"))

(def test-input
  (node-slurp "src/advent2021/day8/day8.test.in"))

(defn parse-line [line]
  (map #(str/split % #" ") (str/split line #" \| ")))

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn process-line [[_ output]]
  (->> output
       (map count)
       (keep #{2 4 3 7})
       (count)))

(defn part1 [input]
  (let [lines (parse-input input)]
    (->> lines
         (map process-line)
         (reduce +))))

(def full-digit-mapping
  {#{"a" "b" "c" "e" "f" "g"} 0
   #{"c" "f"} 1
   #{"a" "c" "d" "e" "g"} 2
   #{"a" "c" "d" "f" "g"} 3
   #{"b" "c" "d" "f"} 4
   #{"a" "b" "d" "f" "g"} 5
   #{"a" "b" "d" "e" "f" "g"} 6
   #{"a" "c" "f"} 7
   #{"a" "b" "c" "d" "e" "f" "g"} 8
   #{"a" "b" "c" "d" "f" "g"} 9})


(defn decode-output [mapping output]
  (let [parsed-digits(map #(->> %
             (map (set/map-invert mapping))
             (set)
             (full-digit-mapping)) output)]
    (->> parsed-digits
         (apply str)
         (js/parseInt))))

(def full-set #{"a" "b" "c" "d" "e" "f" "g"})

(def int-int-mapping
  {#{"c" "f"} "c"
   #{"b" "d"} "d"
   #{"g" "e"} "e"})

(defn get-mapping [digits]
  (let [grouped-digits        (group-by count (map set digits))
        a-mapping             (let [a (apply
                                       set/difference
                                       (map (comp single grouped-digits) [3 2]))]
                                {"a" (single a)})
        intermediary-mapping  (let [[cf] (grouped-digits 2)
                                    bd (apply
                                        set/difference
                                        (map (comp single grouped-digits) [4 2]))
                                    eg (set/difference full-set #{(a-mapping "a")} cf bd)]
                                {#{"c" "f"} cf
                                 #{"b" "d"} bd
                                 #{"e" "g"} eg})
        remaining-matching    (let [sixes (grouped-digits 6)
                                    one-mappings (map (fn [[xs ys]]
                                                        (let [s (int-int-mapping xs)
                                                              sy (some #(when (= (count %) 1) (single %)) (map #(set/intersection ys %) sixes))]
                                                          {(single (set/difference xs #{s})) sy
                                                           s (single (set/difference ys #{sy}))})) intermediary-mapping)]
                                (apply merge one-mappings))]
    (merge a-mapping remaining-matching)))

(defn crack-line [[digits output]]
  (decode-output (get-mapping digits) output))

(defn part2 [input]
  (let [lines (parse-input input)]
    (->> lines
         (map crack-line)
         (reduce +))))

(comment
  (part2 main-input)
  )

(js/console.log
 "Day 8\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))