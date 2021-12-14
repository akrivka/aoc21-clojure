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