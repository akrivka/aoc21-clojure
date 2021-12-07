(ns advent2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (slurp "src/advent2021/day2/day2.in")
       (str/split-lines)))


(defn get-ev [s]
  (let [[type-str val-str] (str/split s #" ")]
    [(keyword type-str) (Integer/parseInt val-str)]))

(defn get-fn1 [[type val]]
  (case type
    :forward #(update % :horiz + val)
    :up #(update % :depth - val)
    :down #(update % :depth + val)))

(defn part1 [input]
  (->> input
  (reduce 
    (fn [pos ev-str] 
      ((get-fn (get-ev ev-str)) pos)) 
    {:horiz 0
     :depth 0})
  (map second)
  (reduce *)))

(defn get-fn2 [[type val]]
  (case type
    :forward 
    #(-> % 
      (update :horiz + val)
      (update :depth + (* val (:aim %))))
    :up #(update % :aim - val)
    :down #(update % :aim + val)))

(defn part2 [input]
  (let [final-pos (->> input
    (reduce 
      (fn [pos ev-str] 
        ((get-fn2 (get-ev ev-str)) pos)) 
      {:horiz 0
       :depth 0
       :aim 0}))]
  (apply * (map final-pos [:horiz :depth]))))

(comment
  input
  (part2 input)
  )