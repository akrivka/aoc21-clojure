(ns advent2021.day9
  (:require [advent2021.util :refer [node-slurp]]
            [clojure.string :as str]))

(def main-input
  (node-slurp "src/advent2021/day9/day9.in"))

(def test-input
  (node-slurp "src/advent2021/day9/day9.test.in"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(map js/parseInt %))
       (map vec)
       (vec)))

(defn get-xy [m [x y]]
  (get-in m [y x]))

(defn is-min [m [x y]]
  (let [h (get-xy m [x y])]
    (every? #(if-let [h' (get-xy m %)]
               (> h' h)
               true) [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])))

(defn part1 [input]
  (let [cave (parse-input input)
        minima (for [x (range (count (first cave)))
                     y (range (count cave))]
                 (when (is-min cave [x y]) (get-xy cave [x y])))]
    (->> minima
         (remove nil?)
         (map inc)
         (reduce +))))

(defn grow [m [x y]]
  (let [h (get-xy m [x y])]
    (keep #(when-let [h' (get-xy m %)]
             (when (and (not= h' 9) (> h' h)) %))
          [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])))

(defn find-basin [m [x y]]
  (assert (is-min m [x y]))
  (loop [[nx :as levels] [#{[x y]}]]
    (let [new-levels (into #{} (mapcat #(grow m %)) nx)]
      (if (empty? new-levels)
        (into #{} (mapcat vec) levels)
        (recur (cons new-levels levels))))))

(defn part2 [input]
  (let [cave (parse-input input)
        minima (remove nil?
                       (for [x (range (count (first cave)))
                             y (range (count cave))]
                         (when (is-min cave [x y]) [x y])))
        basins (map #(find-basin cave %) minima)]
    (->> basins
         (map count)
         (sort)
         (take-last 3)
         (reduce *))))

(grow (parse-input test-input) [1 3])

(comment
  (part1 main-input)
  (part2 main-input)
  
  )

(js/console.log
 "Day 9\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))