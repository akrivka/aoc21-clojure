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