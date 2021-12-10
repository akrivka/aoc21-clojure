(ns advent2021.day10
  (:require [advent2021.util :refer [node-slurp]]
            [clojure.string :as str]))

(def main-input
  (node-slurp "src/advent2021/day10/day10.in"))

(def test-input
  (node-slurp "src/advent2021/day10/day10.test.in"))

(defn parse-input [input]
  (->> input
       (str/split-lines)))

(def matchings
  {\( \)
   \{ \}
   \[ \]
   \< \>})

(def scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def opening (set (keys matchings)))

(def closing (set (vals matchings)))

(defn matches? [o c]
  (= (matchings o) c))

(defn is-illegal? [line]
  (loop [[n & rest] line
         trace      []]
    (if n
      (cond
        (opening n) (recur rest (conj trace n))
        (closing n) (if (matches? (peek trace) n)
                      (recur rest (vec (butlast trace)))
                      n)
        :else "invalid character")
      nil)))

(defn part1 [input]
  (let [lines               (parse-input input)
        illegal-chars       (keep is-illegal? lines)]
    (->> illegal-chars
         (reduce #(let [sum (+ %1 (scores %2))]
                    (js/console.log sum)
                    sum) 0))
    #_(js/console.log (str/join ", " illegal-chars))))

(defn is-incomplete? [line]
  (loop [[n & rest] line
         trace      []]
    (if n
      (cond
        (opening n) (recur rest (conj trace n))
        (closing n) (if (matches? (peek trace) n)
                      (recur rest (vec (butlast trace)))
                      nil)
        :else "invalid character")
      (->> trace 
           (reverse)
           (map matchings)))))

(def scores2
  {\) 1
   \] 2
   \} 3
   \> 4})


(defn part2 [input]
  (let [lines (parse-input input)
        incomplete-lines (keep is-incomplete? lines)
        scores (map (fn [cs] (reduce #(+ (* %1 5) (scores2 %2)) 0 cs)) incomplete-lines)]
    (nth (sort scores) (quot (count scores) 2))))


(comment
  (part2 main-input)
  
  )

(js/console.log
 "Day 10\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))