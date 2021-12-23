(ns advent2021.util
  (:require [clojure.set :as set]))

(defn abs [n]
  (max n (- n)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn single [xs]
  (assert (= (count xs) 1))
  (first xs))

(defn w-some [f coll]
  (some #(when (f %) %) coll))

(defn occurances [coll x]
  (reduce #(if (= %2 x)
             (inc %1)
             %1) 0 coll))

(defn +. [& args]
  (apply + (remove nil? args)))

(defn >. [a b]
  (if (some nil? [a b])
    true
    (> a b)))

(defn <. [a b]
  (if (some nil? [a b])
    true
    (< a b)))

(defn spy [x]
  (println x)
  x)

(defn index-of [pred coll]
  (some (fn [[i x]] (when (pred x) i)) (map-indexed vector coll)))

(index-of #(= % 1) [2 2 3])