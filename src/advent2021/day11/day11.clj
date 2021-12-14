(ns advent2021.day11
  (:require [advent2021.util]
            [clojure.string :as str]))

(def main-input
  (slurp "src/advent2021/day11/day11.in"))

(def test-input
  (slurp "src/advent2021/day11/day11.test.in"))

(def test-input2
  (slurp "src/advent2021/day11/day11.test2.in"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(map (comp parse-long str) %))
       (map vec)
       (vec)))

(defn dmap [f m]
  (map #(map f %) m))

(defn dmapv [f m]
  (mapv #(mapv f %) m))

(defn dmap-indexed [f m]
  (map-indexed (fn [y r] (map-indexed (fn [x e] (f [x y] e)) r)) m))

(defn get-xy [m [x y]]
  (get-in m [y x]))

(defn assoc-xy [m [x y] v]
  (assoc-in m [y x] v))

(defn update-xy [m [x y] f]
  (update-in m [y x] f))

(defn flash [m [x y]]
  (reduce
   #(update-xy %1 %2 inc)
   m
   (for [xx [(dec x) x (inc x)]
         yy [(dec y) y (inc y)]
         :when (and (or (not= xx x) (not= yy y))
                    (get-xy m [xx yy]))]
     [xx yy])))

(defn step [octopi]
  (let [h (count octopi)
        w (count (first octopi))
        whole-set (into #{}
                        (for [x (range w)
                              y (range h)]
                          [x y]))]
    (loop [octopi (dmapv inc octopi)
           flashed #{}]
      (let [new-flashed (into #{}
                              (for [x (range w)
                                    y (range h)
                                    :when (and (> (get-xy octopi [x y]) 9)
                                               (not (flashed [x y])))]
                                [x y]))]
        (if (= flashed whole-set)
          :whole-flash
          (if (not-empty new-flashed)
            (recur (reduce flash octopi new-flashed) (into flashed new-flashed))
            [(dmapv #(if (> % 9) 0 %) octopi)
             (count flashed)]))))))

(defn part1 [input n]
  (let [starting-octopi (parse-input input)]
    (loop [i 0
           octopi starting-octopi
           total-flashes 0]
      (if (< i n)
        (let [[new-octopi new-flashes] (step octopi)]
          (recur (inc i) new-octopi (+ total-flashes new-flashes)))
        total-flashes))))

(defn part2 [input n]
  (let [starting-octopi (parse-input input)]
    (loop [i 0
           octopi starting-octopi
           total-flashes 0]
      (if (< i n)
        (let [step-result (step octopi)]
          (if (= step-result :whole-flash)
            i
            (recur (inc i) (first step-result) (+ total-flashes (second step-result)))))
        total-flashes))))

(comment
  (part1 main-input 100)
  (part2 main-input 1000)
  
  )

(prn
 "Day 11\n"
 "- Part 1: " (part1 main-input 100)
 "- Part 2: " (part2 main-input 1000))