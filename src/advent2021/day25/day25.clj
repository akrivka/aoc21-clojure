(ns advent2021.day25
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def main-input (slurp "src/advent2021/day25/day25.in"))

(def test-input (slurp "src/advent2021/day25/day25.test.in"))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:height (count lines)
     :width (count (first lines))
     :array (->> input
                 (re-seq #".")
                 (map (comp first seq))
                 (into-array Character/TYPE))}))

(defn parse-input-imm [input]
  (->> input
       (str/split-lines)
       (mapv vec)))

(defn aget-xy [{:keys [width array]} [x y]]
  (aget array (+ x (* y width))))

(defn aset-xy [{:keys [width array]} [x y] v]
  (aset array (+ x (* y width)) v))

(defn loop-coord [[width height] [x y]]
  [(mod x width) (mod y height)])

(for [y [4 5 6]
      x [1 2 3]]
  [x y])

(defn move-cucumbers! [{:keys [width height] :as floor}]
  (let [east-to-move (remove nil?
                             (for [y (range height)
                                   x (range width)]
                               (when (and (= (aget-xy floor [x y])         \>)
                                          (= (aget-xy floor (loop-coord
                                                             [width height]
                                                             [(inc x) y]))  \.))
                                 [x y])))
        _ (mapv (fn [[x y]]
                  (aset-xy floor [x y] \.)
                  (aset-xy floor (loop-coord
                                  [width height]
                                  [(inc x) y]) \>)) east-to-move)
        south-to-move (remove nil?
                              (for [x (range width)
                                    y (range height)]
                                (when (and (= (aget-xy floor [x y])         \v)
                                           (= (aget-xy floor (loop-coord
                                                              [width height]
                                                              [x (inc y)]))  \.))
                                  [x y])))
        _ (mapv (fn [[x y]]
                  (aset-xy floor [x y] \.)
                  (aset-xy floor (loop-coord
                                  [width height]
                                  [x (inc y)]) \v)) south-to-move)]
    (when (some not-empty [east-to-move south-to-move])
      floor)))

(defn move-cucumbers! [{:keys [width height] :as floor}]
  (let [east-to-move (remove nil?
                             (for [y (range height)
                                   x (range width)]
                               (when (and (= (aget-xy floor [x y])         \>)
                                          (= (aget-xy floor (loop-coord
                                                             [width height]
                                                             [(inc x) y]))  \.))
                                 [x y])))
        _ (mapv (fn [[x y]]
                  (aset-xy floor [x y] \.)
                  (aset-xy floor (loop-coord
                                  [width height]
                                  [(inc x) y]) \>)) east-to-move)
        south-to-move (remove nil?
                              (for [x (range width)
                                    y (range height)]
                                (when (and (= (aget-xy floor [x y])         \v)
                                           (= (aget-xy floor (loop-coord
                                                              [width height]
                                                              [x (inc y)]))  \.))
                                  [x y])))
        _ (mapv (fn [[x y]]
                  (aset-xy floor [x y] \.)
                  (aset-xy floor (loop-coord
                                  [width height]
                                  [x (inc y)]) \v)) south-to-move)]
    (when (some not-empty [east-to-move south-to-move])
      floor)))

(defn string-floor [{:keys [width height] :as floor}]
  (let [floor' (for [y (range height)]
                 (for [x (range width)]
                   (aget-xy floor [x y])))]
    (->> floor'
         (map #(apply str %))
         (str/join "\n"))))

(defn part1 [input]
  (let [floor     (parse-input input)
        stop-step (loop [i 0]
                    (println "i:" i)
                    (let [floor' (move-cucumbers! floor)]
                      (if floor'
                        (recur (inc i))
                        (do
                          (println "xxx")
                          (recur (inc i))))))]
    stop-step))

(defn get-xy [m [x y]]
  (get-in m [y x]))

(defn assoc-xy [m [x y] v]
  (assoc-in m [y x] v))

(defn move-cucumbers [floor]
  (let [width  (count (first floor))
        height (count floor)
        east-to-move (remove nil?
                             (for [y (range height)
                                   x (range width)]
                               (when (and (= (get-xy floor [x y])         \>)
                                          (= (get-xy floor (loop-coord
                                                            [width height]
                                                            [(inc x) y]))  \.))
                                 [x y])))
        floor' (reduce (fn [fl [x y]]
                         (-> fl
                             (assoc-xy [x y] \.)
                             (assoc-xy (loop-coord
                                        [width height]
                                        [(inc x) y]) \>))) floor east-to-move)
        south-to-move (remove nil?
                              (for [x (range width)
                                    y (range height)]
                                (when (and (= (get-xy floor' [x y])         \v)
                                           (= (get-xy floor' (loop-coord
                                                              [width height]
                                                              [x (inc y)]))  \.))
                                  [x y])))
        floor'' (reduce (fn [fl [x y]]
                          (-> fl
                              (assoc-xy [x y] \.)
                              (assoc-xy (loop-coord
                                         [width height]
                                         [x (inc y)]) \v))) floor' south-to-move)]
    (when (some not-empty [east-to-move south-to-move])
      floor'')))

(defn part1-imm [input]
  (let [initial-floor (parse-input-imm input)
        stop-step     (loop [floor initial-floor
                             i     0]
                        (println i)
                        (let [new-floor (move-cucumbers floor)]
                          (if new-floor
                            (recur new-floor (inc i))
                            (inc i))))]
    stop-step))

(defn part2 [input])

(comment
  (part1-imm main-input)
  ;;
  )

(println
 "Day 25\n"
 "- Part 1: " (part1 main-input)
 "- Part 2: " (part2 main-input))